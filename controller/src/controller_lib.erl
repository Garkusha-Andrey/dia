%% @author Aleksandra
%% @doc @todo Add description to controller_lib.


-module(controller_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_all_diameters/0,
         get_diaconfig/1,
         get_peerId/1,
         get_remotePeerIp/1,
         get_dianode/1,
         get_weight/1,
		 initialize_routing/1,

%% Routing interface
         get_ovsIp/0,
         get_publicIp/0,
         get_ovsMac/0,
         get_extGwMac/0,
         get_instance_mac/1,
         list_instance_weights/0,
         list_instance_weights_stub/0,
         list_instance_weights_stub/1,
         list_connections/0,


         get_localIpandMac/1,
         get_diaLocalIp/1,
         get_diaLocalIpConfig/0,
         get_diaLocalMac/1,
         delete/1,
         get_session_pid/1,
         delete_server_info/1,
         delete_session_data/1,
         get_servers_config/0,
         list_servers/0,
         get_servers_per_node/1
        ]).

%% API for diameter
-export([%% servers Table
		 store_procId/2,
		 store_server_procId/3,
		 get_connection_by_realm/2,
		 
		 %% clients Table
		 store_client_port_ipadd/3,
         delete_client_port_ipaddr/2]).

-include("controller_app.hrl").

get_all_diameters() ->
    F = fun() ->
                mnesia:all_keys(diaLocalConfig)
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            io:format("ERROR: Immpossible to get diaLocalConfig due to ~p~n",[Reason]);
        {atomic, ResultOfFun} ->
            lists:append(ResultOfFun)
    end.

list_connections() ->
    lists:map(fun(S) ->
		      #clients{portIpAddr = S#servers.portIpAddr,
			       nodeId = S#servers.nodeId}
	      end, list_servers())
	++ list_clients().

    %%[#servers{portIpAddr = {50000, "1.2.3.4"},
    %%          nodeId = dia1}].

get_instance_mac(NodeId) ->
%%    lists:flatten(lists:duplicate(5,"0" ++ integer_to_list(InstanceId) ++ ":"))
%%      ++ "0" ++ integer_to_list(InstanceId).
    get_diaLocalMac(NodeId).


get_diaconfig() ->
    F = fun() ->
                AllDistanceID = mnesia:all_keys(diaConnections),
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
            error_logger:error_report("ERROR: Immpossible to get diaConnections due to ~p",[Reason]);
        {atomic, ResultOfFun} ->
            lists:append(ResultOfFun)
    end.

get_diaconfig(InstanceID) ->
    F = fun() ->
                AllDistanceID = mnesia:all_keys(diaConnections),
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
            error_logger:error_report("ERROR: Immpossible to get diaConnections due to ~p",[Reason]);
        {atomic, ResultOfFun} ->
            lists:append(ResultOfFun)
    end.

get_weight(InstanceID) ->
    F = fun() ->
                AllDistanceID = mnesia:all_keys(instanceWeight),
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
            error_logger:error_report("ERROR: Immpossible to get instanceWeight due to ~p",[Reason]);
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
    DiaConfigR#diaConnections.nodeId.
        
get_localIpandMac(InstanceID) ->
    F = fun() ->
                AllDistanceID = mnesia:all_keys(diaLocalConfig),
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
            error_logger:report("ERROR: Immpossible to get diaLocalConfig due to ~p",[Reason]);
        {atomic, ResultOfFun} ->
            [DiaLocalIPR] = lists:append(ResultOfFun),
            {DiaLocalIPR#diaLocalConfig.ipAddress, DiaLocalIPR#diaLocalConfig.macAddress}
    end.

get_diaLocalIpConfig() ->
    F = fun() ->
                AllDistanceID = mnesia:all_keys(diaLocalConfig),
                lists:map(fun(Elem) ->
                                  mnesia:read(diaLocalConfig, Elem)
                          end,
                          AllDistanceID)
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to get diaLocalConfig due to ~p",[Reason]);
        {atomic, ResultOfFun} ->
            lists:append(ResultOfFun)
    end.

%%Gets IP of diameter instance per NodeId:
get_diaLocalIp(NodeId) ->
    Records = get_diaLocalIpConfig(),
    lists:filtermap(fun(RElem) ->
                            if RElem#diaLocalConfig.nodeId == NodeId ->
                                    {true, RElem#diaLocalConfig.ipAddress};
                               true ->
                                    false
                            end
                    end,
                    Records).

%%Gets Mac of diameter instance per NodeId:
get_diaLocalMac(NodeId) ->
    Records = get_diaLocalIpConfig(),
    lists:flatten(lists:filtermap(fun(RElem) ->
                            if RElem#diaLocalConfig.nodeId == NodeId ->
                                    {true, RElem#diaLocalConfig.macAddress};
                               true ->
                                    false
                            end
                    end,
                    Records)).

get_GlobalData() ->
    F = fun() ->
                [OvsIp] = mnesia:all_keys(globalData),
                mnesia:read(globalData, OvsIp)
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to get globalData due to ~p",[Reason]);
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

list_instance_weights() ->
    lists:map(fun(D) ->
                      #instanceWeight{nodeId=D#diaLocalConfig.nodeId,
                                      weight=100}
              end, get_diaLocalIpConfig()).

list_instance_weights_stub() ->
    list_instance_weights_stub(2).

list_instance_weights_stub(Iteration) ->

    case Iteration of
        1 ->
%% empty instance list with no previous instances
            [];
        2 ->
%% 3 even instances with 8/3 = 2,66 chunks each
            [#instanceWeight{nodeId=1, weight=95},
             #instanceWeight{nodeId=2, weight=100},
             #instanceWeight{nodeId=3, weight=100}];
        3 ->
%% empty instance list with previous instances
            [];
        4 ->
%% 6 even instances with 8/6 = 1,33 chunks each
            [#instanceWeight{nodeId=1, weight=100},
             #instanceWeight{nodeId=2, weight=95},
             #instanceWeight{nodeId=3, weight=100},
             #instanceWeight{nodeId=4, weight=100},
             #instanceWeight{nodeId=5, weight=100},
             #instanceWeight{nodeId=6, weight=100}];
        5 ->
            [#instanceWeight{nodeId=2, weight=80},
             #instanceWeight{nodeId=3, weight=20}];
        _ ->
            exit(ok)
    end.

delete(Node) ->
    delete_diaconnections(Node),
    delete_diaLocalConfig(Node),
    delete_from_servers(Node).

delete_from_servers(Node) ->
    F = fun() -> 
                Keys = mnesia:all_keys(servers),
                lists:foreach(fun(Key) ->
                                      [R] = mnesia:wread({servers, Key}),
                                      if R#servers.nodeId == Node ->
                                              mnesia:write(R#servers{nodeId = undefined});
                                         true ->
                                              do_nothing
                                      end
                              end,
                              Keys)             
        end,
        Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to delete server due to ~p",[Reason]);
        {atomic, ResultOfFun} ->
            ResultOfFun
    end.

delete_diaconnections(Node) ->
    F = fun() -> 
                AllInstances = mnesia:all_keys(diaConnections),
                lists:map(fun(#diaConnections{nodeId = NodeId} = Elem) when NodeId == Node ->
                                  [Record] = mnesia:read(diaConnections, Elem),
                                  mnesia:delete_object(Record)
                          end,
                          AllInstances)         
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to get diaConnections due to ~p",[Reason]);
        {atomic, ResultOfFun} ->
            lists:append(ResultOfFun)
    end.

delete_diaLocalConfig(Node) ->
    check_transaction(mnesia:transaction(fun() ->
			mnesia:delete({diaLocalConfig, Node})
					 end),
		      "Impossible to delete from diaLocalConfig").

get_instance_by_node(Node) ->
    F = fun() ->
                DiaConfKeys = mnesia:all_keys(diaConnections),
                lists:foldl(fun(Elem, Acc) ->
                                    [Record] = mnesia:read(diaConnections, Elem),
                                    case Record#diaConnections.nodeId of
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
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("Impossible to get the dia instances from "
                                      "diaConfig table due to reason: ~p",[Reason]);
        {atomic, ResultOfFun} ->
            ResultOfFun
    end.

get_session_pid(Node) ->
    F = fun() ->
                ServersKeys = mnesia:all_keys(servers),
                lists:foldl(fun(Elem, Acc) ->
                                    [Record] = mnesia:read(servers, Elem),
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
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("Impossible to get the servers config from "
                                      "servers table due to reason: ~p",[Reason]);
        {atomic, ResultOfFun} ->
            ResultOfFun
    end.

delete_session_data(Node) ->
    SessionPids = get_session_pid(Node),
    lists:foreach(fun(Pid) ->
                          {Pid, Node} ! terminate
                  end,
                  SessionPids),
    delete_session(Node).

delete_session(Node) ->
    F = fun() ->
                ServersKeys = mnesia:all_keys(servers),
                lists:foreach(fun(Elem, Acc) ->
                                      [Record] = mnesia:read(servers, Elem),
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
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("Impossible to get the servers config from "
                                                                          "servers table due to reason: ~p",[Reason]);
        {atomic, ResultOfFun} ->
            ResultOfFun
    end.

delete_server_info(Node) ->
    F = fun() ->
                ServersKeys = mnesia:all_keys(servers),
                lists:foreach(fun(Elem) ->
                                      [Record] = mnesia:read(servers, Elem),
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
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("Impossible to get the servers config from "
                                      "servers table due to reason: ~p",[Reason]);
        {atomic, ResultOfFun} ->
            ResultOfFun
    end.


get_servers_config() ->
    mnesia:lock({table, server}, read).

list_servers() ->
    F = fun() ->
                PortIp = mnesia:all_keys(servers),
                RecordList  = lists:foldl(fun(Elem, Acc) ->
                                                  Acc ++ mnesia:read(servers, Elem)
                                          end,
                                          [],
                                          PortIp),
                DistRecordList =lists:filter(fun(#servers{nodeId = NodeId}) ->
                                                     NodeId /= undefined
                                             end,
                                             RecordList),
                lists:sort(fun(#servers{realmId = RealmIdA}, #servers{realmId = RealmIdB}) ->
                                   RealmIdA =< RealmIdB
                           end,
                           DistRecordList)
                                                
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to get servers due to ~p~n",[Reason]);
        {atomic, Servers} ->
            Servers
    end.

get_servers_per_node(NodeId) ->
    F = fun() ->
                PortIp = mnesia:all_keys(servers),
                lists:foldl(fun(Elem, Acc) ->
                                    [R] = mnesia:read(servers, Elem),
                                    case R#servers.nodeId of
                                        NodeId ->
                                            [R|Acc];
                                        _ ->
                                            Acc
                                    end
                            end,
                            [],
                            PortIp)
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to get servers due to ~p~n",[Reason]);
        {atomic, Servers} ->
            Servers
    end.

get_server(Port, IpAddress) ->
    F = fun() ->
                mnesia:read(servers, {Port, IpAddress})
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to get servers due to ~p~n",[Reason]);
        {atomic, Server} ->
            Server
    end.

store_procId(Server, ProcessId) ->
    Key = Server#servers.portIpAddr,
    F = fun() ->
                [R] = mnesia:wread({servers, Key}),
                mnesia:write(R#servers{processId = ProcessId})
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: Immpossible to"
                                      " get servers due to ~p~n",[Reason]);
        {atomic, Res} ->
            Res
    end.

store_server_procId(Port, IpAddress, ProcessId) ->
    [Server] = get_server(Port, IpAddress),
    store_procId(Server, ProcessId).

get_connection_by_realm(RealmHost, RealmId) ->
	io:fwrite("get_connection_by_realm RealmHost ~w and RealmId ~w ~n",
									  [RealmHost, RealmId]),
	    F = fun() ->
                ServersKeys = mnesia:all_keys(servers),
                lists:foldl(fun(Elem, Acc) ->
                                    [Record] = mnesia:read(servers, Elem),
                                    if
                                        {Record#servers.realmHost, Record#servers.realmId} == {RealmId, RealmHost} ->
                                            [{Record#servers.processId,Record#servers.nodeId}|Acc];
                                        true -> Acc
                                    end
                            end,
                            [],
                            ServersKeys)                
        end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("Impossible to get the servers connections to "
                                      "servers with RealmHost ~p and RealmId ~p due to ~p~n",
									  [RealmHost, RealmId, Reason]);
        {atomic, ResultOfFun} ->
            ResultOfFun
    end.

store_client_port_ipadd(Port, IpAddress, NodeId) ->
    F = fun() ->
                mnesia:write(#clients{portIpAddr = {Port, IpAddress},
				      nodeId = NodeId})
        end,
    mnesia:activity(transaction, F).

delete_client_port_ipaddr(Port, IpAddress) ->
    F = fun() ->
                [ClientRecord] = mnesia:read(clients, {Port, IpAddress}),
                mnesia:delete_object(ClientRecord)
        end,
    mnesia:activity(transaction, F).

list_clients() ->
    F = fun() ->
                lists:map(fun(Key) ->
				  lists:last(mnesia:read(clients, Key))
			  end, mnesia:all_keys(clients))
        end,
    check_transaction(mnesia:transaction(F), "Failed to get clients").


check_transaction(Result, ErrMsg) ->
    case Result of
        {aborted, Reason} ->
            error_logger:error_report("ERROR: ~s. Reason: ~p~n",
				      [ErrMsg, Reason]);
        {atomic, Data} ->
            Data
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% ====================================================================
%% Initializes the routing.
%% If the initialization has failed, wait 15 sec. and try again.
%% Error will be detected if routing has not initialized in 5 minutes.
%% ====================================================================
initialize_routing(0) ->
	error_logger:error_report("ERROR: Routing has not been initialized in 10 retries!!!");
initialize_routing(Tries) ->
	case routing:init() of
		tryagain ->
			timer:sleep(15000),
			initialize_routing(Tries - 1);
		_ ->
			%%Initialized:
            ok
	end.