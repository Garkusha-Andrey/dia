%% @author Aleksandra
%% @doc @todo Add description to controller_lib.


-module(controller_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_diaconfig/1,
		 get_peerId/1,
		 get_diaIpandMacIp/1,
		 get_remotePeerIp/1,
		 get_dianode/1,
		 get_weight/1,
		 get_diaIp/1,
		 get_ovsIp/0,
		 get_ovsMacIp/0,
		 get_ovsPublicIp/0,
		 get_extGMacIp/0,
		 instance_weights_get/0,
		 instance_weights_get/1,
		 get_instance_mac/1,
		 get_connections/0,
		 get_localIpandMacIp/1,
		 get_diaLocalIp/1,
		 get_diaLocalIpConfig/0,
		 get_diaLocalMacIp/1,
		 get_diaMacIp/1,
		 get_diaIpConfig/0,
		 delete/1]).

-include("controller_app.hrl").

get_connections() ->
	get_diaconfig().

get_instance_mac(InstanceId) ->
    lists:flatten(lists:duplicate(5,integer_to_list(InstanceId) ++ ":"))
	++ integer_to_list(InstanceId).


get_diaconfig() ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaConfig),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) ->							
							  Record = mnesia:read(diaConfig, Elem),
							  [Record | Acc]
					  end,
					[],
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

get_diaconfig(InstanceID) ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaConfig),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) when Elem == InstanceID ->							
							  Record = mnesia:read(diaConfig, Elem),
							  [Record | Acc];
					   (_Elem,Acc) ->
							Acc
					  end,
					[],
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

get_weight(InstanceID) ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(instanceWeight),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) when Elem == InstanceID ->							
							  Record = mnesia:read(instanceWeight, Elem),
							  [Record | Acc];
					   (_Elem,Acc) ->
							Acc
					  end,
					[],
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			[InstWeigth] = lists:append(ResultOfFun),
			InstWeigth#instanceWeight.weight
	end.
	
get_peerId(InstanceID) ->
	[DiaConfigR] = get_diaconfig(InstanceID),
	DiaConfigR#diaConfig.peerId.

get_remotePeerIp(InstanceID) ->
	[DiaConfigR] = get_diaconfig(InstanceID),
	DiaConfigR#diaConfig.remotePeerIp.

get_dianode(InstanceID) ->
	[DiaConfigR] = get_diaconfig(InstanceID),
	DiaConfigR#diaConfig.node.
	
get_localIpandMacIp(InstanceID) ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaLocalIpAddress),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) when Elem == InstanceID ->							
							  Record = mnesia:read(diaLocalIpAddress, Elem),
							  [Record | Acc];
					   (_Elem,Acc) ->
							Acc
					  end,
					[],
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			[DiaLocalIPR] = lists:append(ResultOfFun),
			{DiaLocalIPR#diaLocalIpAddress.ipAddress, DiaLocalIPR#diaLocalIpAddress.macIpAddress}
	end.

get_diaIpandMacIp(InstanceID) ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaIpAddress),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) when Elem == InstanceID ->							
							  Record = mnesia:read(diaIpAddress, Elem),
							  [Record | Acc];
					   (_Elem,Acc) ->
							Acc
					  end,
					[],
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			[DiaIpR] = lists:append(ResultOfFun),
			{DiaIpR#diaIpAddress.ipAddress, DiaIpR#diaIpAddress.macIpAddress}
	end.

get_diaIpConfig() ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaIpAddress),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:map(fun(Elem) ->
						  mnesia:read(diaIpAddress, Elem)
					  end,
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

get_diaLocalIpConfig() ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaLocalIpAddress),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:map(fun(Elem) ->
						  mnesia:read(diaLocalIpAddress, Elem)
					  end,
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaLocalIpAddress due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

get_diaIp(InstanceId) ->
	Records = get_diaIpConfig(),
	lists:filtermap(fun(RElem) ->
						if RElem#diaIpAddress.diaInstanceId == InstanceId ->
							   {true, RElem#diaIpAddress.ipAddress};
						   true ->
							   flse
						end
					end,
					Records).

get_diaMacIp(InstanceId) ->
	Records = get_diaIpConfig(),
	lists:filtermap(fun(RElem) ->
						if RElem#diaIpAddress.diaInstanceId == InstanceId ->
							   {true, RElem#diaIpAddress.macIpAddress};
						   true ->
							   flse
						end
					end,
					Records).

get_diaLocalIp(InstanceId) ->
	Records = get_diaLocalIpConfig(),
	lists:filtermap(fun(RElem) ->
						if RElem#diaLocalIpAddress.diaInstanceId == InstanceId ->
							   {true, RElem#diaLocalIpAddress.ipAddress};
						   true ->
							   flse
						end
					end,
					Records).

get_diaLocalMacIp(InstanceId) ->
	Records = get_diaLocalIpConfig(),
	lists:filtermap(fun(RElem) ->
						if RElem#diaLocalIpAddress.diaInstanceId == InstanceId ->
							   {true, RElem#diaLocalIpAddress.macIpAddress};
						   true ->
							   flse
						end
					end,
					Records).

get_GlobalData() ->
	F = fun() ->
				[OsvIp] = mnesia:all_keys(globalData),
				io:format("Key of GlobalData:OSV ~p~n",[OsvIp]),
				mnesia:read(globalData, OsvIp)
		end,
							  
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, GlobalDataRec} ->
			GlobalDataRec
	end.


get_ovsPublicIp() ->
	GlobalDataRec = get_GlobalData(),
	GlobalDataRec#globalData.publicIp.

get_ovsMacIp() ->
	GlobalDataRec = get_GlobalData(),
	GlobalDataRec#globalData.ovsMacIp.

get_ovsIp() ->
	GlobalDataRec = get_GlobalData(),
	GlobalDataRec#globalData.ovsIp.

get_extGMacIp() ->
	GlobalDataRec = get_GlobalData(),
	GlobalDataRec#globalData.extGMacIp.

instance_weights_get() ->
        instance_weights_get(1).

instance_weights_get(Iteration) ->

        case Iteration of
	    1 ->
%% empty instance list with no previous instances
	        [];
	    2 ->
%% 3 even instances with 8/3 = 2,66 chunks each
                [#instanceWeight{diaInstanceId=1, weight=95},
                 #instanceWeight{diaInstanceId=2, weight=100},
                 #instanceWeight{diaInstanceId=3, weight=100}];
	    3 ->
%% empty instance list with previous instances
		[];
	    4 ->
%% 6 even instances with 8/6 = 1,33 chunks each
                [#instanceWeight{diaInstanceId=1, weight=100},
                 #instanceWeight{diaInstanceId=2, weight=95},
                 #instanceWeight{diaInstanceId=3, weight=100},
                 #instanceWeight{diaInstanceId=4, weight=100},
                 #instanceWeight{diaInstanceId=5, weight=100},
                 #instanceWeight{diaInstanceId=6, weight=100}];
	    5 ->
			[#instanceWeight{diaInstanceId=2, weight=80},
			 #instanceWeight{diaInstanceId=3, weight=20}];
	    _ ->
		  exit(ok)
     end.

delete(Node) ->
	io:format("xkulale: in delete!~n"),
	delete_diaconfig(Node),
	delete_diaLocalIpAddress(Node),
	delete_diaIpAddress(Node).

delete_diaconfig(Node) ->
	Instances = get_instance_by_node(Node),
	F = fun() -> 
				lists:flatmap(fun(Elem) ->
						  Record = mnesia:read(diaConfig, Elem),
					      mnesia:delete_object(Record)
					  end,
					Instances)		
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

delete_diaLocalIpAddress(Node) ->
	Instances = get_instance_by_node(Node),
	F = fun() ->
		lists:flatmap(fun(Elem) ->							
							  Record = mnesia:read(diaLocalIpAddress, Elem),
							  mnesia:delete_object(Record)
					  end,
					Instances)
		
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

delete_diaIpAddress(Node) ->
	Instances = get_instance_by_node(Node),
	io:format("XKULALE: Instances ~p~n",[Instances]),
	case Instances of
		[] ->
			do_nothing;
		_ ->
			F = fun() ->
						lists:flatmap(fun(Elem) ->							
							  Record = mnesia:read(diaIpAddress, Elem),
     						  mnesia:delete_object(Record)
					  end,
					Instances)
				end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end
	end.

get_instance_by_node(Node) ->
	F = fun() ->
		DiaConfKeys = mnesia:all_keys(diaConfig),
		io:format("DiaConfKeys Keys is ~p~n",[DiaConfKeys]),
		lists:foldl(fun(Elem, Acc) ->							
							  Record = mnesia:read(diaConfig, Elem),
							  case Record#diaConfig.node of
								  Node ->
									  [Record|Acc];
								  _ ->
									  ok
							  end

					  end,
					DiaConfKeys)		
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			error_logger:error_report("Impossible to get the dia instances from "
									  "diaConfig table due to reason: ~p",[Reason]);
		{atomic, ResultOfFun} ->
			io:format("XKULALE: Result ~p~n",[ResultOfFun]),
			lists:append(ResultOfFun)
	end.





%% ====================================================================
%% Internal functions
%% ====================================================================
