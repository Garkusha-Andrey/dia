%% Inbound relay


-module(i_relay_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-include_lib("../../controller/src/controller_app.hrl").
-include_lib("dia_relay_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).


-define(UNEXPECTED, erlang:error({unexpected, ?MODULE, ?LINE})).

-define(LISTENER_PROCESS, irelay_l).

%% ====================================================================
%% Callback implementation
%% ====================================================================

peer_up(_SvcName, _Peer, State) ->
	io:format("i_relay_cb::peer_up~n"),
    State.

peer_down(_SvcName, _Peer, State) ->
	io:format("i_relay_cb::peer_down~n"),
    State.

pick_peer(_, _, _SvcName, _State) ->
    ?UNEXPECTED.

prepare_request(_, _SvcName, _Peer) ->
    ?UNEXPECTED.

prepare_retransmit(_Packet, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_answer(_Packet, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_error(_Reason, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

%% A request whose decode was successful ...
handle_request(#diameter_packet{header = Header, msg = Req, errors = []} = RcvRequestPkt, _SvcName, {_, Caps})
  when is_record(Req, diameter_base_RAR) ->
	io:format("i_relay_cb::handle_request RAR diameter message\n"),
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #diameter_base_RAR{'Session-Id' = Id,
                       'Re-Auth-Request-Type' = Type}
        = Req,
	#diameter_header{hop_by_hop_id = HopByHopId,
					 end_to_end_id = EndToEndId}
		= Header,
	
	io:fwrite("i_relay_cb:: handle_request ~p \n", [Req]),
	io:fwrite("i_relay_cb:: HopByHopId: ~p, EndToEndId: ~p \n", [HopByHopId, EndToEndId]),
	
	AnswerHdr = #diameter_header{%%hop_by_hop_id = HopByHopId,
				end_to_end_id = EndToEndId},
	RAA = #diameter_base_RAA{'Result-Code' = rc(Type),
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR,
                               'Session-Id' = Id},
	
	TemplateAnswerPkt = #diameter_packet{header = AnswerHdr,
							  msg = RAA},

	
	%% Will try to resend it to relay_outbound
	ORelayAnswerPkt = pass_to_orelay(RcvRequestPkt, Caps),

	%%compile Answer Packet
	AnswerPkt = compile_answer_packet(ORelayAnswerPkt, TemplateAnswerPkt),

    {reply, AnswerPkt};

%% ... or one that wasn't. 3xxx errors are answered by diameter itself
%% but these are 5xxx errors for which we must contruct a reply.
%% diameter will set Result-Code and Failed-AVP's.
handle_request(#diameter_packet{msg = Req}, _SvcName, {_, Caps})
  when is_record(Req, diameter_base_RAR) ->
	io:format("i_relay_cb::handle_request got RAR msg 2"),
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #diameter_base_RAR{'Session-Id' = Id}
        = Req,

    {reply, #diameter_base_RAA{'Origin-Host' = OH,
                               'Origin-Realm' = OR,
                               'Session-Id' = Id}};

%% Answer that any other message is unsupported.
handle_request(#diameter_packet{}, _SvcName, _) ->
	io:format("i_relay_cb::handle_request got npn RAR msg"),
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

%% Map Re-Auth-Request-Type to Result-Code just for the purpose of
%% generating different answers.

rc(0) ->
    2001;  %% DIAMETER_SUCCESS
rc(_) ->
    5012.  %% DIAMETER_UNABLE_TO_COMPLY


%% ====================================================================
%% Internal functions
%% ====================================================================

pass_to_orelay(Pkt = #diameter_packet{}, Caps = #diameter_caps{}) ->
	io:fwrite("i_relay_cb::pass_to_orelay ~n"),
	ListenerProcess = 'create_procee_name',
	register(ListenerProcess, self()),
	
	ORelay = lookup_orelay(Pkt, Caps, stub),
	send_reques_to_orelay(ListenerProcess, ORelay, Pkt),
	
	AnswerPkt = wait_for_orelay_answer(),
	AnswerPkt;
pass_to_orelay(_Pkt, _Caps) ->
	io:fwrite("ERROR: Something goes wrong! pass_to_orelay() didn't parse message ~n").


%% lookup_relay_outbound/1
lookup_orelay(#diameter_packet{msg = _Req}, Caps) ->
	#diameter_caps{ origin_host = {OH,_},
                   	origin_realm = {OR,_}}
        = Caps,
	
	Controller = get_controller_node_name(),
	Server = rpc:call(Controller, controller_lib, get_server_by_host_realm, [OH, OR]),
		
	#relay{ node_name = Server#servers.nodeId,
			process_name = Server#servers.processId};

lookup_orelay(_Pkt, _Caps) ->
	io:fwrite("ERROR: Something goes wrong! lookup_relay_outbound() didn't parse message ~n").

lookup_orelay(#diameter_packet{msg = _Req}, _Caps, stub) ->
	[{active, NodeName, ProcessName}] = ets:lookup(next_hope, active),
	io:fwrite("I`ve chosen a orelay: ~w ~w ~n", [NodeName, ProcessName]),
	
	#relay{ process_name = ProcessName,
			node_name 	 = NodeName}.


%% send_reques_to_orelay/3
send_reques_to_orelay(ListenerProcess,
					  #relay{ process_name = ProcessName, node_name = NodeName},
					  #diameter_packet{} = Pkt) ->
	Request = #payload_request{direction = 'to_orelay',
							   src_node_name = node(),
							   rcv_process_name = ListenerProcess},
	{ProcessName, NodeName} ! {Request, Pkt},
	ok.


wait_for_orelay_answer() ->
	receive
		{payload_answer_from_orelay, Pkt}
		    when is_record(Pkt, diameter_packet)->
			io:fwrite("I`ve got an answer payload message from orelay.~n"
					  "~p ~n", [Pkt]),
			Pkt;
		{payload_answer_from_orelay, Something} ->
			io:format("Answer payload from orelay is undefined ~n"
					  "~p ~n", [Something]),
			error;
		_ ->
			io:format("I can't recognize the message: ~n")
	end.


compile_answer_packet(_ORelayAnswerPkt, TemplateAnswerPkt) ->
	TemplateAnswerPkt.


get_controller_node_name() ->
	ok.