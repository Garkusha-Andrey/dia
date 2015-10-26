-module(dia_stubs).

-export([instance_weights_get/0,
	 instance_weights_get/1,
	 connections_get/0]).

-include("dia_stubs.hrl").

instance_weights_get() ->
        instance_weights_get(1).

instance_weights_get(Iteration) ->

        case Iteration == 1 of
                true ->

%% Case 1: 3 even instances with 8/3 = 2,66 chunks each
%%                [#instanceWeight{id=1, weight=95},
%%                 #instanceWeight{id=2, weight=100},
%%                 #instanceWeight{id=3, weight=100}];

%% Case 2: 6 even instances with 8/6 = 1,33 chunks each
                [#instanceWeight{id=1, weight=100},
                 #instanceWeight{id=2, weight=95},
                 #instanceWeight{id=3, weight=100},
                 #instanceWeight{id=4, weight=100},
                 #instanceWeight{id=5, weight=100},
                 #instanceWeight{id=6, weight=100}];

%% Case 3: empty instance list
%%	        [];
        false ->
%% Empty instance list
%%		[]
%% Normal weights
		[%%#instanceWeight{id=1, weight=50},
                 #instanceWeight{id=2, weight=80},
                 #instanceWeight{id=3, weight=20}]
     end.

connections_get() ->
    [#diaConfig{peerId="peer1",
                remotePeerIp="10.0.0.2",
                diaInstanceId=2},
     #diaConfig{peerId="peer2",
                remotePeerIp="10.0.0.4",
                diaInstanceId=2},
     #diaConfig{peerId="peer3",
                remotePeerIp="10.0.0.6",
                diaInstanceId=1}
     ].
