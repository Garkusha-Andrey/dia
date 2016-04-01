%% @author Aleksandra
%% @doc @todo Add description to controller_lib.


-module(controller_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_diaconfig/1,
		 get_peerId/1,
		 get_remotePeerIp/1,
		 get_dianode/1,
		 get_weight/1,

		 get_ovsIp/0,
		 get_publicIp/0,
		 get_ovsMac/0,
		 get_extGwMac/0,

		 instance_weights_get/0,
		 instance_weights_get/1,
		 get_instance_mac/1,
		 get_connections/0,
		 get_localIpandMacIp/1,
		 get_diaLocalIp/1,
		 get_diaLocalIpConfig/0,
		 get_diaLocalMacIp/1,
		 delete/1,
		 get_session_pid/1,
		 delete_server_info/1,
		 delete_session_data/1]).

-include("controller_app.hrl").

get_connections() ->
    %% xantnef 01/04/2016 temporarily stubbed out
    %%get_diaconfig().
    [#servers{ipaddress = "1.2.3.4",
	      port = 50000,
	      nodeId = 1}].

get_instance_mac(InstanceId) ->
    lists:flatten(lists:duplicate(5,"0" ++ integer_to_list(InstanceId) ++ ":"))
	++ "0" ++ integer_to_list(InstanceId).


get_diaconfig() ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaConnections),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) ->							
							  Record = mnesia:read(diaConnections, Elem),
							  [Record | Acc]
					  end,
					[],
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConnections due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

get_diaconfig(InstanceID) ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaConnections),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) when Elem == InstanceID ->							
							  Record = mnesia:read(diaConnections, Elem),
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
			io:format("ERROR: Immpossible to get diaConnections due to ~p~n",[Reason]);
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
			io:format("ERROR: Immpossible to get instanceWeight due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			[InstWeigth] = lists:append(ResultOfFun),
			InstWeigth#instanceWeight.weight
	end.
	
get_peerId(InstanceID) ->
	[DiaConfigR] = get_diaconfig(InstanceID),
	DiaConfigR#diaConnections.peerId.

get_remotePeerIp(InstanceID) ->
	[DiaConfigR] = get_diaconfig(InstanceID),
	DiaConfigR#diaConnections.remotePeerIp.

get_dianode(InstanceID) ->
	[DiaConfigR] = get_diaconfig(InstanceID),
	DiaConfigR#diaConnections.node.
	
get_localIpandMacIp(InstanceID) ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaLocalConfig),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:foldl(fun(Elem, Acc) when Elem == InstanceID ->							
							  Record = mnesia:read(diaLocalConfig, Elem),
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
			io:format("ERROR: Immpossible to get diaLocalConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			[DiaLocalIPR] = lists:append(ResultOfFun),
			{DiaLocalIPR#diaLocalConfig.ipAddress, DiaLocalIPR#diaLocalConfig.macIpAddress}
	end.

get_diaLocalIpConfig() ->
	F = fun() ->
		AllDistanceID = mnesia:all_keys(diaLocalConfig),
		io:format("ALl Keys is ~p~n",[AllDistanceID]),
		lists:map(fun(Elem) ->
						  mnesia:read(diaLocalConfig, Elem)
					  end,
					AllDistanceID)
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaLocalConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

get_diaLocalIp(InstanceId) ->
	Records = get_diaLocalIpConfig(),
	lists:filtermap(fun(RElem) ->
						if RElem#diaLocalConfig.diaInstanceId == InstanceId ->
							   {true, RElem#diaLocalConfig.ipAddress};
						   true ->
							   flse
						end
					end,
					Records).

get_diaLocalMacIp(InstanceId) ->
	Records = get_diaLocalIpConfig(),
	lists:filtermap(fun(RElem) ->
						if RElem#diaLocalConfig.diaInstanceId == InstanceId ->
							   {true, RElem#diaLocalConfig.macIpAddress};
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
			io:format("ERROR: Immpossible to get globalData due to ~p~n",[Reason]);
		{atomic, GlobalDataRec} ->
			GlobalDataRec
	end.


get_publicIp() ->
	[GlobalDataRec] = get_GlobalData(),
	GlobalDataRec#globalData.publicIpMask.

get_ovsMac() ->
	[GlobalDataRec] = get_GlobalData(),
	GlobalDataRec#globalData.ovsMac.

get_ovsIp() ->
	[GlobalDataRec] = get_GlobalData(),
	GlobalDataRec#globalData.ovsIpMask.

get_extGwMac() ->
	[GlobalDataRec] = get_GlobalData(),
	GlobalDataRec#globalData.extGwMac.

instance_weights_get() ->
        instance_weights_get(2).

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
	Instances = get_instance_by_node(Node),
	delete_diaconfig(Instances),
	io:format("Before delete diaLocalConfig~n"),
	delete_diaLocalIpAddress(Instances).

delete_diaconfig(Instances) ->
	io:format("in delete_diaconfig ~n"),
	
	io:format("Instances is ~p~n",[Instances]),
	F = fun() -> 
				lists:map(fun(Elem) ->
						  [Record] = mnesia:read(diaConnections, Elem),
						  io:format("Elem is ~p, Record is ~p~n",[Elem,Record]),
					      mnesia:delete_object(Record)
					  end,
					Instances)		
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaConnections due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

delete_diaLocalIpAddress(Instances) ->
	F = fun() ->
		lists:map(fun(Elem) ->							
							  [Record] = mnesia:read(diaLocalConfig, Elem),
							  mnesia:delete_object(Record)
					  end,
					Instances)
		
		end,
	Result = mnesia:transaction(F),
	case Result of
		{aborted, Reason} ->
			io:format("ERROR: Immpossible to get diaLocalConfig due to ~p~n",[Reason]);
		{atomic, ResultOfFun} ->
			lists:append(ResultOfFun)
	end.

get_instance_by_node(Node) ->
	io:format("i am in get_instance_by node~n"),
	F = fun() ->
		DiaConfKeys = mnesia:all_keys(diaConnections),
		io:format("diaConnections Keys is ~p~n",[DiaConfKeys]),
		lists:foldl(fun(Elem, Acc) ->							
							  [Record] = mnesia:read(diaConnections, Elem),
							  io:format("get instance by node, Record is ~p, Elem is ~p~n",[Record, Elem]),
							  case Record#diaConnections.node of
								  Node ->
									  [Elem|Acc];
								  _ ->
									  Acc
							  end

					  end,
					[],
					DiaConfKeys)		
		end,
	Result = mnesia:transaction(F),
	io:format("Result is ~p~n",[Result]),
	case Result of
		{aborted, Reason} ->
			error_logger:error_report("Impossible to get the dia instances from "
									  "diaConfig table due to reason: ~p",[Reason]);
		{atomic, ResultOfFun} ->
			io:format("XKULALE: ResultofFun ~p~n",[ResultOfFun]),
			ResultOfFun
	end.

get_session_pid(Node) ->
	F = fun() ->
		ServersKeys = mnesia:all_keys(servers),
		io:format("ServersKeys is ~p~n",[ServersKeys]),
		lists:foldl(fun(Elem, Acc) ->							
							  [Record] = mnesia:read(servers, Elem),
							  io:format("get instance by node, Record is ~p, Elem is ~p~n",[Record, Elem]),
							  case Record#servers.nodeId of
								  Node ->
									  [Record#servers.processId|Acc];
								  _ ->
									  Acc
							  end

					  end,
					[],
					ServersKeys)		
		end,
	Result = mnesia:transaction(F),
	io:format("Result is ~p~n",[Result]),
	case Result of
		{aborted, Reason} ->
			error_logger:error_report("Impossible to get the servers config from "
									  "servers table due to reason: ~p",[Reason]);
		{atomic, ResultOfFun} ->
			io:format("XKULALE: ResultofFun ~p~n",[ResultOfFun]),
			ResultOfFun
	end.

delete_session_data(Node) ->
	SessionPid = get_session_pid(Node),
	{SessionPid, Node} ! terminate,
	delete_session(Node).

delete_session(Node) ->
	F = fun() ->
		ServersKeys = mnesia:all_keys(servers),
		io:format("ServersKeys is ~p~n",[ServersKeys]),
		lists:foreach(fun(Elem, Acc) ->							
							  [Record] = mnesia:read(servers, Elem),
							  io:format("get record by node, Record is ~p, Elem is ~p~n",[Record, Elem]),
							  case Record#servers.nodeId of
								  Node ->
									  mnesia:delete_object(Record);
								  _ ->
									  Acc
							  end

					  end,
					[],
					ServersKeys)		
		end,
	Result = mnesia:transaction(F),
	io:format("Result is ~p~n",[Result]),
	case Result of
		{aborted, Reason} ->
			error_logger:error_report("Impossible to get the servers config from "
									  "servers table due to reason: ~p",[Reason]);
		{atomic, ResultOfFun} ->
			io:format("XKULALE: ResultofFun ~p~n",[ResultOfFun]),
			ResultOfFun
	end.

delete_server_info(Node) ->
	F = fun() ->
		ServersKeys = mnesia:all_keys(servers),
		io:format("ServersKeys is ~p~n",[ServersKeys]),
		lists:foreach(fun(Elem) ->							
							  [Record] = mnesia:read(servers, Elem),
							  io:format("get record by node, Record is ~p, Elem is ~p~n",[Record, Elem]),
							  case Record#servers.nodeId of
								  Node ->
									  NewRecord = Record#servers{nodeId = undefined,
																 processId = undefined},
									  mnesia:write(NewRecord);
								  _ ->
									  do_nothing
							  end

					  end,
					ServersKeys)		
		end,
	Result = mnesia:transaction(F),
	io:format("Result is ~p~n",[Result]),
	case Result of
		{aborted, Reason} ->
			error_logger:error_report("Impossible to get the servers config from "
									  "servers table due to reason: ~p",[Reason]);
		{atomic, ResultOfFun} ->
			io:format("XKULALE: ResultofFun ~p~n",[ResultOfFun]),
			ResultOfFun
	end.




%% ====================================================================
%% Internal functions
%% ====================================================================
