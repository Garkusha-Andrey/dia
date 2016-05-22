%% @author agarkush
%% @doc @todo Add description to orelay.


-module(orelay).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-include_lib("dia_relay_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%-export([deploy/1,
%		 start/2,     %% start a service
%         start/3,     %%
%         connect/2,   %% add a connecting transport
%         call/2,      %% send using the record encoding
%         cast/1,      %% send using the list encoding and detached
%         stop/1]).    %% stop a service

-export([init/0]).

-export([deploy/1,
         stop/1]).    %% stop a service
		 
%% special for orelay
-export([call/3,
		 listen_for_request/1]).

-define(DEF_SVC_NAME, ?MODULE).
-define(IRELAY_CALLBACK_MOD, o_relay_cb).
-define(L, atom_to_list).

-define(LISTENER_PROCESS, irelay_l).

-define(SERVICE(HostName, Realm),
						[{'Origin-Host', ?L(HostName) ++ "." ++ ?L(Realm)},
                        {'Origin-Realm', ?L(Realm)},
                        {'Vendor-Id', 0},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [0]},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, ?IRELAY_CALLBACK_MOD}]}]).

%% ====================================================================
%% API functions
%% ====================================================================

init() ->
	ets:new(next_hope_relay, [end_to_end, node_name, process_id]),
	ok.

%% deploy/1 ([<Name>, <Ralm>, <local IP>, <remote IP>, <Port>])
%% deploy([c1, 'ex.ru', {127,0,0,1}, {127,0,0,1}, 3911]).
deploy(T) ->
	{ok, Log} = file:open("orelay.log", [append]),
	erlang:group_leader(Log, self()),

	Name = lists:nth(1, T),
	Realm = lists:nth(2, T),
	LIp = lists:nth(3, T),
	RIp = lists:nth(4, T),
	Port = lists:nth(5, T),

    diameter:start(),
	
	start(Name, Realm),
    connect(Name, {tcp, LIp, RIp, Port})
	.

%% start/2
start(Name, Realm)
  when is_atom(Name) ->
    start(Name, Realm, []).


%% start/3
start(Name, Realm, Opts) ->
    node:start(Name, Opts ++ [T || {K,_} = T <- ?SERVICE(Name, Realm),
                                   false == lists:keymember(K, 1, Opts)]).

%% connect/2
connect(Name, {tcp, _LIp, RIp, Port} = T) ->
    Connection = node:connect(Name, T),
	case Connection of
		{ok, _}		-> io:format("orelay: connect. ~p connected to server ~p:~w ~n", [node(), RIp, Port]),
					   orelay_listener(Name, RIp, Port);
		{error, _} 	-> io:format("orelay: connect. ~p failed connection to server ~p:~w ~n", [node(), RIp, Port])
	end
.

%% call/1
%% Send base RAR message to server
call(Name, rar) ->
    SId = diameter:session_id(?L(Name)),
    RAR = #diameter_base_RAR{'Session-Id' = SId,
                             'Auth-Application-Id' = 0,
                             'Re-Auth-Request-Type' = 0},
	io:format("orelay::call(Name)~n SId: ~s\n", [SId]),
    diameter:call(Name, common, RAR, []).

%% Special function for orelay
call(Name, orelay, Pkt=#diameter_packet{}) ->
	io:format("orelay::call is ready ~n"),
    SId = diameter:session_id(?L(Name)),

	io:format("orelay::call(Name)~n SId: ~s\n", [SId]),
    Answer = diameter:call(Name, common, Pkt, []),
	case Answer of
		{error, Reason} ->
			io:format("orelay: Error happaned: the reason is: ~p", [Reason]);
		AnswerPkt ->
			io:format("orelay: I've got answer from server~n"),
			AnswerPkt
	end,
	Answer.

%% cast/1

cast(Name) ->
    SId = diameter:session_id(?L(Name)),
    RAR = ['RAR', {'Session-Id', SId},
                  {'Auth-Application-Id', 0},
                  {'Re-Auth-Request-Type', 1}],
	io:format("orelay::cast(Name)~n SId: ~s\n", [SId]),
    diameter:call(Name, common, RAR, [detach]).

%% stop/1

stop(Name) ->
    node:stop(Name).


%% ====================================================================
%% Internal functions
%% ====================================================================
orelay_listener(Name, RIp, Port) ->
	ListenerProcessName = list_to_atom(lists:concat(["listener_or_",Name])),
	register(ListenerProcessName, spawn(?MODULE, listen_for_request, [Name])),
	
	io:format("orelay:: Name ~w, Process ~w ~n", [Name, ListenerProcessName]),
	controller_lib:store_server_procId(Port, inet_parse:ntoa(RIp), ListenerProcessName),
	ok.

listen_for_request(Name) ->
	io:format("orelay:: waiting for request ~w ~n", [Name]),
	receive
		{PayloadRequest, Pkt}
		    when is_record(Pkt, diameter_packet)->
			io:format("orelay:: I've got request payload from irelay.~n"
					  "~p ~n", [Pkt]),

			PreparedPkt		= prepare_pkt(Pkt),
			{ok, AnswerPkt}	= call(Name, orelay, PreparedPkt),

			IRelay = #relay{ node_name = PayloadRequest#payload_request.src_node_name,
							 process_name = PayloadRequest#payload_request.rcv_process_name },
			
			%%IRelay = lookup_for_irelay(AnswerPkt),
			send_answer_to_irelay(IRelay, AnswerPkt);

		{payload_request_to_irelay, Something} ->
			io:format("orelay:: Request payload from irelay incorrect. ~n"
					  "~p ~n", [Something]);

		_ ->
			io:format("WARNING: recaived strange msg. ~n")
	end,
	
	listen_for_request(Name).

prepare_pkt(Pkt = #diameter_packet{}) ->
	Header	= #diameter_header{hop_by_hop_id = Pkt#diameter_packet.header#diameter_header.hop_by_hop_id,
							   end_to_end_id = Pkt#diameter_packet.header#diameter_header.end_to_end_id},

	PreparedPkt = #diameter_packet{header 	= Header,
								   msg		= Pkt#diameter_packet.msg },
	
	PreparedPkt.


send_answer_to_irelay(IRelay = #relay{},
					  Pkt 	 = #diameter_packet{}) ->
	io:format("orelay:: I`m going to send answer to node: ~w ~n"
			  "process: ~w ~n", [IRelay#relay.node_name, IRelay#relay.process_name]),
	{IRelay#relay.process_name, IRelay#relay.node_name} ! {payload_answer_from_orelay, Pkt}.
