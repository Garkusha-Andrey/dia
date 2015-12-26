-module(dia_stubs).

-export([instance_weights_get/0,
	 instance_weights_get/1,
	 connections_get/0,
	 get_instance_mac/1]).

-include("dia_stubs.hrl").

instance_weights_get() ->
        instance_weights_get(1).

instance_weights_get(Iteration) ->

        case Iteration of
	    1 ->
%% empty instance list with no previous instances
	        [];
	    2 ->
%% 3 even instances with 8/3 = 2,66 chunks each
                [#instanceWeight{id=1, weight=95},
                 #instanceWeight{id=2, weight=100},
                 #instanceWeight{id=3, weight=100}];
	    3 ->
%% empty instance list with previous instances
		[];
	    4 ->
%% 6 even instances with 8/6 = 1,33 chunks each
                [#instanceWeight{id=1, weight=100},
                 #instanceWeight{id=2, weight=95},
                 #instanceWeight{id=3, weight=100},
                 #instanceWeight{id=4, weight=100},
                 #instanceWeight{id=5, weight=100},
                 #instanceWeight{id=6, weight=100}];
	    5 ->
		[#instanceWeight{id=2, weight=80},
                 #instanceWeight{id=3, weight=20}];
	    _ ->
		  exit(ok)
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

get_instance_mac(InstanceId) ->
    lists:flatten(lists:duplicate(5,integer_to_list(InstanceId) ++ ":"))
	++ integer_to_list(InstanceId).
