%% @author agarkush
%% @doc @todo Add description to orelay.


-module(orelay).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([deploy/1,
		 start/2,     %% start a service
         start/3,     %%
         connect/2,   %% add a connecting transport
         call/2,      %% send using the record encoding
         cast/1,      %% send using the list encoding and detached
         stop/1]).    %% stop a service
		 
%% special for orelay
-export([call/3,
		 listen_for_request/1]).

-define(DEF_SVC_NAME, ?MODULE).
-define(IRELAY_CALLBACK_MOD, o_relay_cb).
-define(L, atom_to_list).

-record(irelay, {process_name,
				 node_name		}).

%% The service configuration. As in the server example, a client
%% supporting multiple Diameter applications may or may not want to
%% configure a common callback module on all applications.
-define(SERVICE(HostName, Realm),
						[{'Origin-Host', ?L(HostName) ++ "." ++ ?L(Realm)},
                        {'Origin-Realm', ?L(Realm)},
                        {'Vendor-Id', 0},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [0]},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, ?IRELAY_CALLBACK_MOD}]}]).
%% deploy/1
%% deploy([<Name>, <Ralm>, <local IP>, <remote IP>, <Port>])
%% deploy([c1, 'ex.ru', {127,0,0,1}, {127,0,0,1}, 3911]).
deploy(T) ->
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
connect(Name, T) ->
    Connection = node:connect(Name, T),
	case Connection of
		{ok, _}		-> irelay_listener(Name);
		{error, _} 	-> io:fwrite("ORelay not connected to server")
	end
.

%% call/1
%% Send base RAR message to server
call(Name, rar) ->
    SId = diameter:session_id(?L(Name)),
    RAR = #diameter_base_RAR{'Session-Id' = SId,
                             'Auth-Application-Id' = 0,
                             'Re-Auth-Request-Type' = 0},
	io:format("client.erl::call(Name)~n SId: ~s\n", [SId]),
    diameter:call(Name, common, RAR, []).

%% Special function for orelay
call(Name, orelay, Pkt=#diameter_packet{}) ->
	io:fwrite("orelay::call is ready ~n"),
    SId = diameter:session_id(?L(Name)),

	io:format("orelay.erl::call(Name)~n SId: ~s\n", [SId]),
    Answer = diameter:call(Name, common, Pkt, []),
	case Answer of
		{error, Reason} ->
			io:fwrite("Error happaned: the reason is: ~p", [Reason]);
		AnswerPkt ->
			io:fwrite("I've got answer from server~n"),
			AnswerPkt
	end,
	Answer.

%% cast/1

cast(Name) ->
    SId = diameter:session_id(?L(Name)),
    RAR = ['RAR', {'Session-Id', SId},
                  {'Auth-Application-Id', 0},
                  {'Re-Auth-Request-Type', 1}],
	io:format("client.erl::cast(Name)~n SId: ~s\n", [SId]),
    diameter:call(Name, common, RAR, [detach]).

%% stop/1

stop(Name) ->
    node:stop(Name).


%% ====================================================================
%% Internal functions
%% ====================================================================
irelay_listener(Name) ->
	io:fwrite("Start of irelay_listener ~n"),
	register(irelay_l, spawn(?MODULE, listen_for_request, [Name])),
	ok.

listen_for_request(Name) ->
	io:fwrite("Start of listen_for_request ~n"),
	receive
		{payload_request_to_irelay, Pkt}
		    when is_record(Pkt, diameter_packet)->
			io:fwrite("I've got request payload from irelay.~n"
					  "~p ~n", [Pkt]),

			PreparedPkt		= prepare_pkt(Pkt),
			{ok, AnswerPkt}	= call(Name, orelay, PreparedPkt),

			IRelay = lookup_for_irelay(AnswerPkt),
			send_answer_to_irelay(IRelay, AnswerPkt);

		{payload_request_to_irelay, Something} ->
			io:fwrite("Request payload from irelay incorrect. ~n"
					  "~p ~n", [Something]);

		_ ->
			io:fwrite("WARNING: recaived strange msg. ~n")
	end.

prepare_pkt(Pkt = #diameter_packet{}) ->
	Header	= #diameter_header{hop_by_hop_id = Pkt#diameter_packet.header#diameter_header.hop_by_hop_id,
							   end_to_end_id = Pkt#diameter_packet.header#diameter_header.end_to_end_id},
	Msg 	= Pkt#diameter_packet.msg,

	PreparedPkt = #diameter_packet{header = Header,
								   msg = Msg },
	io:fwrite("Prepare_pk passed successful ~n"),
	PreparedPkt.


send_answer_to_irelay(IRelay, Pkt = #diameter_packet{}) ->
	{IRelay#irelay.process_name, IRelay#irelay.node_name} ! Pkt
	;
send_answer_to_irelay(_Relay, _Pkt)->
	io:fwrite("Trying to return something strange ~n").


lookup_for_irelay(_Pkt = #diameter_packet{}) ->
	#irelay{ process_name	= way_back,
			 node_name		= 'ir1@agarkush-VirtualBox'};
lookup_for_irelay(Something) ->
	io:fwrite("Strange input for 'lookup_for_irelay' ~n"
			  "~p ~n", [Something]).