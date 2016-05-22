%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2014. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(client_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

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

peer_up(_SvcName, {_, Caps}, State) ->
	#diameter_caps{origin_host = {OH, DH},
                   host_ip_address = {IPsrc, IPdst}}
        = Caps, 
	io:format("client: connection Up. ~p -> ~p ~n"
			  "                       ~p -> ~p ~n",
			  [OH, DH, IPsrc, IPdst]),
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
	io:format("client: connection Down.~n~n"),
    State.

%% pick_peer/4

pick_peer([Peer | _], _, _SvcName, _State) ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(#diameter_packet{msg = ['RAR' = T | Avps]}, _, {_, Caps}) ->
	io:format("client.cb::prepare_request 1~n"),
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
	

    {send, [T, {'Origin-Host', OH},
               {'Origin-Realm', OR},
               {'Destination-Host', DH},
               {'Destination-Realm', DR}
             | Avps]};

%% client:call(c1, user, "s1", "ex.com").
prepare_request(#diameter_packet{msg = Rec} = _Pkt, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, _DH},
                   origin_realm = {OR, _DR}}
        = Caps,

	Msg = Rec#diameter_base_RAR{'Origin-Host' = OH,
                                 'Origin-Realm' = OR},

	case {Msg#diameter_base_RAR.'Destination-Host', Msg#diameter_base_RAR.'Destination-Realm'} of
		{undefined, undefined} ->
			io:format("Destination is empty~n");
		{_OHost, _ORealm} ->
			ok
			%%io:format("Destination is not empty.~n Host ~p Realm: ~p~n", [OHost, ORealm])
	end,

    {send, Msg}.

%% prepare_retransmit/3

prepare_retransmit(Packet, SvcName, Peer) ->
    prepare_request(Packet, SvcName, Peer).

%% handle_answer/4 

handle_answer(#diameter_packet{header = Header, msg = Msg}, _Request, _SvcName, _Peer) ->
	#diameter_base_RAA{'Session-Id' = Id}
			= Msg,
	#diameter_header{hop_by_hop_id = HopByHopId,
					 end_to_end_id = _EndToEndId}
			= Header,

	io:format("client: Answer recieved [1] ~n"
			 "             SessionId:   ~p ~n"
			 "             HopByHopId:  ~p ~n",
			 [Id, HopByHopId]),
	
    {ok, Msg}.

%% handle_error/4

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(_Packet, _SvcName, _Peer) ->
	io:format("client.cb::handle_request. ERROR~n"),
    erlang:error({unexpected, ?MODULE, ?LINE}).
