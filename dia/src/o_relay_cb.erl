%% Outbound relay

-module(o_relay_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

-include_lib("dia_relay_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

%% peer_up/3

peer_up(_SvcName, {_,Caps}, State) ->
    error_logger:info_msg("irelay_cb::peer_up ~p connected to client ~p ~w ~n"
						 , [node(), Caps#diameter_caps.origin_host, Caps#diameter_caps.host_ip_address]),
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    error_logger:info_msg("orelay_cb::peer_down ~p connection down with server ~n", [node()]),
    State.

%% pick_peer/4

pick_peer([Peer | _], _, _SvcName, _State) ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(#diameter_packet{msg = ['RAR' = T | Avps]}, _, {_, Caps}) ->
    error_logger:info_msg("orelay_cb::prepare_request 1~n"),
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
	

    {send, [T, {'Origin-Host', OH},
               {'Origin-Realm', OR},
               {'Destination-Host', DH},
               {'Destination-Realm', DR}
             | Avps]};

prepare_request(#diameter_packet{msg = Rec}, _, {_, Caps}) ->
    error_logger:info_msg("orelay_cb::prepare_request 2~n"),
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,

    {send, Rec#diameter_base_RAR{'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Destination-Host' = DH,
                                 'Destination-Realm' = DR}}.

%% prepare_retransmit/3

prepare_retransmit(Packet, SvcName, Peer) ->
    prepare_request(Packet, SvcName, Peer).

%% handle_answer/4 

handle_answer(#diameter_packet{header = Header, msg = Msg} = Pkt, _Request, _SvcName, _Peer) ->
    error_logger:info_msg("orelay_cb::handle_answer:~n ~p~n", [Msg]),
    #diameter_header{hop_by_hop_id = HopByHopId,
                     end_to_end_id = EndToEndId}
	= Header,
	
    error_logger:info_msg("orelay_cb:: HopByHopId: ~p, EndToEndId: ~p~n",
			  [HopByHopId, EndToEndId]),
	
    %%{ok, Msg}.
	{ok, Pkt}.

%% handle_error/4

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(_Packet, _SvcName, _Peer) ->
    error_logger:info_msg("orelay_cb::handle_request~n"),
    erlang:error({unexpected, ?MODULE, ?LINE}).



%% ====================================================================
%% Internal functions
%% ====================================================================
