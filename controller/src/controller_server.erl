%% @author Aleksandra
%% @doc @todo Add description to controller_server.
-module(controller_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, check_tables/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2,initial_distribution/0,
	 get_nodes_from_servers/1,get_nodes/0, clients_updated/0,
	 check_distribution/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([change_configuration/1]).
-include("controller_app.hrl").

%% ====================================================================
%% Behavioural functions
%% ====================================================================
%-record(state, {}).


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
        Result :: {ok, State}
                        | {ok, State, Timeout}
                        | {ok, State, hibernate}
                        | {stop, Reason :: term()}
                        | ignore,
        State :: term(),
        Timeout :: non_neg_integer() | infinity.

%% ====================================================================
init([]) ->
    error_logger:info_report("An initialization of an Erlang controller server!"),
    net_kernel:monitor_nodes(true),
    {ok, []}.

%%Start Appl server:
start_link() ->
    case global:whereis_name(?MODULE) of
          undefined ->
                gen_server:start_link({global, ?MODULE}, ?MODULE, [], []);
          Pid ->
             error_logger:info_msg("The old Erlang controller server is still running!"
				"The old server Pid is ~p~n",[Pid]),
             global:unregister_name(?MODULE),
             timer:sleep(5000),
             gen_server:start_link({global, ?MODULE}, ?MODULE, [], [])
    end.

%%Stop Appl server:
stop() ->
    gen_server:call({global,?MODULE}, stop).

%%Cast to server to configure the instances:
change_configuration(Args) ->
    error_logger:info_msg("Before cast new_config with ARgs ~p~n",[Args]),
    gen_server:cast({global,?MODULE}, {new_config, Args}).


%%Call to Server to distribute the diamters per server (30 sec is enough?):
initial_distribution() ->
    error_logger:info_msg("Before call for an initial distribution of servers!~n"),
    gen_server:call({global,?MODULE}, initial_distribution, 30000).

%%Cast to server to tell to routing about updated clients:
clients_updated() ->
    gen_server:cast({global,?MODULE}, clients_updated).

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
        Result :: {reply, Reply, NewState}
                        | {reply, Reply, NewState, Timeout}
                        | {reply, Reply, NewState, hibernate}
                        | {noreply, NewState}
                        | {noreply, NewState, Timeout}
                        | {noreply, NewState, hibernate}
                        | {stop, Reason, Reply, NewState}
                        | {stop, Reason, NewState},
        Reply :: term(),
        NewState :: term(),
        Timeout :: non_neg_integer() | infinity,
        Reason :: term().
%% ====================================================================
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({check_tables, Node}, _From, State) ->
    Answer = check_tables(Node),
    {reply, Answer, State};
handle_call(initial_distribution, _From, State) ->
    error_logger:info_msg("HANDLE_CALL for initial distribution!~n"),
    Result = initial_servers_distribution(),
    {reply, Result, State};
handle_call(_Call, _From, State) ->
    {noreply, State}.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
        Result :: {noreply, NewState}
                        | {noreply, NewState, Timeout}
                        | {noreply, NewState, hibernate}
                        | {stop, Reason :: term(), NewState},
        NewState :: term(),
        Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({new_config,[diameter, Enode, LocalIp, LocalMac] =Args}, State) ->
    error_logger:info_msg("Configuration for DIA node is received with Args ~p~n",[Args]),
    %%Configure of just started diameter instance:
    F = fun() ->
		mnesia:write(
			  #diaConnections{nodeId = Enode}),
			mnesia:write(
			  #diaLocalConfig{nodeId = Enode,
					  ipAddress  = LocalIp,
					  macAddress = atom_to_list(LocalMac)})
	end,
	Result = mnesia:activity(transaction, F),
	error_logger:info_msg("Result of transaction is ~p~n",[Result]),
	{noreply, State};
handle_cast({new_config,[OVSIntIp, OVSIntMask, PublicIp,
			 PublicMask, OVSMac, ExtGwMac]}, State) ->
    %%configure Global Data for controller:
    F = fun() ->
		mnesia:write(
			  #globalData{ovsIpMask = {atom_to_list(OVSIntIp),
						   atom_to_list(OVSIntMask)},
                                      publicIpMask = {atom_to_list(PublicIp),
				                      atom_to_list(PublicMask)},
				      ovsMac   = atom_to_list(OVSMac),
				      extGwMac = atom_to_list(ExtGwMac)})
	end,
	mnesia:activity(transaction, F),
	%%Initialize the routing:
	controller_lib:initialize_routing(20),
	{noreply, State};
handle_cast(clients_updated, State) ->
    %%Updating of routing:
    routing:update(),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
        Result :: {noreply, NewState}
                        | {noreply, NewState, Timeout}
                        | {noreply, NewState, hibernate}
                        | {stop, Reason :: term(), NewState},
        NewState :: term(),
        Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({nodeup, Node}, State) ->
    error_logger:info_msg("Node ~p is up!~n",[Node]),
    rpc:call(Node, application, start, [mnesia]),
    rpc:call(Node, application, start, [inets]),
    check_tables(Node),
    NodeL = atom_to_list(Node),
    case string:str(NodeL, "diameter") of
            1 ->
        	ets:insert(diaNodes, {node, Node});
	    0 ->
		case string:str(NodeL, "controllerA") of
			1 ->
	                    do_nothing;
			0 ->
			    case string:str(NodeL, "controller") of
			            1 ->
					rpc:call(Node, controller_app, start,
						 [{takeover, Node}, []]);
				    0 ->
					do_nothing
			    end
		end
    end,
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    error_logger:info_msg("Node ~p is down!~n",[Node]),
    NodeL = atom_to_list(Node),
    case string:str(NodeL, "diameter") of
	1 ->
            ets:delete(diaNodes, Node),
	    BServers = controller_lib:list_servers(),
	    controller_lib:delete(Node),
	    redistribute_servers(),
	    AServers = controller_lib:list_servers(),
	    check_distr(BServers, AServers),
	    routing:update();
	0 ->
	    do_nothing
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
        Reason :: normal
                        | shutdown
                        | {shutdown, term()}
                        | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
        Result :: {ok, NewState :: term()} | {error, Reason :: term()},
        OldVsn :: Vsn | {down, Vsn},
        Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%%Distribute the nesia tables for new node:
check_tables(Node) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
		{ok, [Node]} ->
			case catch mnesia:table_info(diaConnections, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaConnections,
							[{record_name, diaConnections},
							 {attributes, record_info(fields, diaConnections)},
							 {ram_copies, Node}]),
					mnesia:add_table_copy(diaConnections, Node, ram_copies);
				_ ->
					mnesia:add_table_copy(diaConnections, Node, ram_copies)
			end,
			case catch mnesia:table_info(diaLocalConfig, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaLocalConfig,
							[{record_name, diaLocalConfig},
							 {attributes, record_info(fields, diaLocalConfig)},
							 {ram_copies,  Node}]);
				_ ->
					mnesia:add_table_copy(diaLocalConfig, Node, ram_copies)
			end,
			case catch mnesia:table_info(globalData, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(globalData,
							[{record_name, globalData},
							 {attributes, record_info(fields, globalData)},
							 {ram_copies,  Node}]);
				_ ->
					mnesia:add_table_copy(globalData, Node, ram_copies)
			end,
			case catch mnesia:table_info(instanceWeight, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(instanceWeight,
							[{record_name, instanceWeight},
							 {attributes, record_info(fields, instanceWeight)},
							 {ram_copies,  Node}]);
				_ ->
					mnesia:add_table_copy(instanceWeight, Node, ram_copies)
			end,
			case catch mnesia:table_info(servers, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(servers,
							[{record_name, servers},
							 {attributes, record_info(fields, servers)},
							 {ram_copies,  Node}]);
				_ ->
					mnesia:add_table_copy(servers, Node, ram_copies)
			end,
			case catch mnesia:table_info(clients, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(clients,
							[{record_name, clients},
							 {attributes, record_info(fields, clients)},
							 {ram_copies,  Node}]);
				_ ->
					mnesia:add_table_copy(clients, Node, ram_copies)
			end;
		_ ->
			do_nothing
	end.

%%Re-distribution of diameters per servers:
redistribute_servers() ->
    Servers = get_not_distributed_servers(),
    error_logger:info_msg("redistribute_servers: Non-distributed servers are ~p~n",[Servers]),
    %Nodes = get_nodes(),
    DiaNodes = controller_lib:get_all_diameters(),
    error_logger:info_msg("redistribute_servers: All DIA nodes are ~p~n",[DiaNodes]),
    Nodes  = lists:sort(fun(NodeA, NodeB) ->
					NodeA =< NodeB
			end,
			DiaNodes),
    error_logger:info_msg("redistribute_servers: Sorted DIA nodes are ~p~n",[Nodes]),
    case Nodes of
	[] ->
		error_logger:info_msg("redistribute_servers: No any DIA node. Nothing to distribute!~n");
	_ ->
		NodeSumL = get_nodes_from_servers(Nodes),
		error_logger:info_msg("redistribute_servers: map Node-Servers ~p~n",[NodeSumL]),
		NewNodeSumL = lists:keysort(2,lists:keysort(1, NodeSumL)),
		error_logger:info_msg("redistribute_servers: Sorted map Node-Servers ~p~n",[NodeSumL]),
		NewNodes = [K || {K, _} <- NewNodeSumL],
		distribute_server_per_node(Servers, NewNodes)
    end.

%%Helper function for redistribute_servers/1:
get_nodes_from_servers(Nodes) ->
    lists:foldr(
	  fun(Node, Acc) ->
			  F = fun() ->
				  Keys = mnesia:all_keys(servers),
				  Sum = lists:foldl(
						  fun(Key, Acc1) ->
								  [R] = mnesia:wread({servers, Key}),
								  NodeId = R#servers.nodeId,
								  if NodeId == Node ->
									 Acc1 + 1;
								 true ->
									 Acc1
								  end
						  end,
						  0,
						  Keys),
						  {Node, Sum}
			  end,
			  Result = mnesia:transaction(F),
			  case Result of
				  {aborted, Reason} ->
					  error_logger:error_msg("get_nodes_from_servers: Immpossible "
							   "to get map Node -Server ininfo_msgion due to ~p~n",
							   [Reason]),
					  Acc;
				  {atomic, ResultOfFun} ->
					  [ResultOfFun | Acc]
			  end
	  end,
	  [],
	  Nodes).


%%Initial distribution of diameters per server:
initial_servers_distribution() ->
    error_logger:info_msg("An initial Server distribution is started!~n"),
    DistrServers = controller_lib:list_servers(),
    error_logger:info_msg("initial_servers_distribution: All distributed "
                          "Servers are ~p!~n",[DistrServers]),
    Servers = case DistrServers of
		  [] ->
			  %%Servers do not exist or no any servers distributed:
			  error_logger:info_msg("initial_servers_distribution: No any "
						 "server has been distributed!~n"),
			  get_not_distributed_servers();
		  _ ->
                      %%Re-distribution all servers:
                      AllServers = controller_lib:get_all_servers(),
		      error_logger:info_msg("initial_servers_distribution: "
		                            "All Servers are ~p~n",[AllServers]),
                      AllServers
					 
        end,
    error_logger:info_msg("initial_servers_distribution: Servers "
			   "which should be distributed ~p!~n",[Servers]),
    %Nodes = get_nodes(),
    DiaNodes = controller_lib:get_all_diameters(),
    error_logger:info_msg("initial_servers_distribution: All DIA nodes are ~p~n",[DiaNodes]),
    Nodes  = lists:sort(fun(NodeA, NodeB) ->
					NodeA =< NodeB
			end,
			DiaNodes),
    error_logger:info_msg("initial_servers_distribution: Sorted DIA nodes are ~p~n",[Nodes]),
    case Nodes of
	[] ->
            error_logger:error_msg("initial_servers_distribution: Servers could not be distributed."
	                           " No any DIA nodes exist!~n");
	_ ->
	    distribute_server_per_node(Servers, Nodes)
    end,
    RedistrServers = controller_lib:list_servers(),
    error_logger:info_msg("initial_servers_distribution: Servers "
			   "after distribution are ~p~n",[RedistrServers]),
    check_distr(DistrServers, RedistrServers),
    routing:update().

%%Helper functions for initial_servers_distribution/0:
distribute_server_per_node([], _Nodes) ->
    error_logger:info_msg("distribute_server_per_node:done~n"),
    ok;
distribute_server_per_node([Server | _Servers] = TotServers, Nodes) ->
    error_logger:info_msg("distribute_server_per_node: the disttribution "
			   "Server ~p for Nodes ~p~n",[Server,Nodes]),
    RealmId = Server#servers.realmId,
    error_logger:info_msg("distribute_server_per_node: RealmId ~p~n",[RealmId]),
    TmpServers = get_all_servers_realm(RealmId),
    error_logger:info_msg("distribute_server_per_node: Server ~p with RealmId ~p~n",[Server,TmpServers]),
    NewNodes = distribute_servers(TmpServers, Nodes),
    error_logger:info_msg("distribute_server_per_node: NewNodes ~p~n",[NewNodes]),
    NewServers = TotServers -- TmpServers,
    error_logger:info_msg("distribute_server_per_node: NewServers ~p~n",[NewServers]),
    distribute_server_per_node(NewServers,NewNodes).

distribute_servers([], Nodes) ->
    Nodes;
distribute_servers([Server | Servers], [Node | Nodes]) ->
    store_nodeId(Server, Node),
    distribute_servers(Servers, Nodes ++ [Node]).

get_nodes() ->
    Nodes = ets:foldl(fun({node, NodeId}, Acc) ->
					  [NodeId | Acc]
    		  end,
		  [],
		  diaNodes),
    lists:sort(fun(NodeA, NodeB) ->
				   NodeA =< NodeB
	   end,
	   Nodes).

%%Store distribution into servers table:
store_nodeId(Server, NodeId) ->
    error_logger:info_msg("store_nodeId: Server ~p is stored for DIA node ~p~n",[Server, NodeId]),
    Key = Server#servers.portIpAddr,
    F = fun() ->
		[R] = mnesia:wread({servers, Key}),
		mnesia:write(R#servers{nodeId = NodeId})
     end,
    Result = mnesia:transaction(F),
    case Result of
	{aborted, Reason} ->
	    error_logger:error_msg("store_nodeId:  Immpossible to"
                               " store Server ~p per Node ~p due to ~p~n",[Server, NodeId, Reason]);
	{atomic, Table} ->
	    Table
    end.

%%Helper functions:
get_all_servers_realm(RealmId) ->
    F = fun() ->
		PortIp = mnesia:all_keys(servers),
		RecordList = lists:foldl(fun(Elem, Acc) ->
							 Acc ++ mnesia:read(servers, Elem)
					 end,
					 [],
					 PortIp),
		RecList =lists:filter(fun(#servers{realmId = RId}) ->
								  RealmId == RId
				  end,
				  RecordList),
		lists:sort(fun(#servers{realmHost = HostA}, #servers{realmHost = HostB}) ->
					   HostA =< HostB
			   end,
			   RecList)
    end,
    Result = mnesia:transaction(F),
    case Result of
	{aborted, Reason} ->
		error_logger:error_msg("get_all_servers_realm: Immpossible to get all servers "
					 "by realmId ~p due to ~p",[RealmId,Reason]);
	{atomic, Servers} ->
		Servers
    end.

%%Get all not yet distributed servers:
get_not_distributed_servers() ->
    F = fun() ->
		PortIp = mnesia:all_keys(servers),
		RecordList = lists:foldl(fun(Elem, Acc) ->
						  Acc ++ mnesia:read(servers, Elem)
					  end,
					  [],
					  PortIp),
		NotDistRecordList = lists:filter(fun(#servers{nodeId = NodeId}) ->
								 NodeId == undefined
						 end,
						 RecordList),
		lists:sort(fun(#servers{realmId = RealmIdA}, #servers{realmId = RealmIdB}) ->
					   RealmIdA =< RealmIdB
			   end,
			   NotDistRecordList)
    end,
    Result = mnesia:transaction(F),
    case Result of
 	{aborted, Reason} ->
		error_logger:error_msg("Immpossible to get not distributed "
				 "servers due to ~p~n",[Reason]);
	{atomic, Servers} ->
		Servers
    end.

%%Terminate TCP connection if distribution changed:
check_distr([], []) ->
    error_logger:info_msg("Check distribution is comlete~n"),
    ok;
check_distr([],_) ->
    error_logger:info_msg("An initial distribution! Check is not needed!~n"),
    ok;
check_distr([BServer | BServers], [AServer | AServers]) ->
    error_logger:info_msg("check_distribution: "
			 "BServers ~p; AServers ~p~n",[BServers, AServers]),
    error_logger:info_msg("check_distribution: "
			 "AServer ~p, BServer ~p~n",[AServer,BServer]),
    #servers{portIpAddr = {BPort, BIp}} = BServer,
    #servers{portIpAddr = {APort, AIp}} = AServer,
    BNode = BServer#servers.nodeId,
    ANode =AServer#servers.nodeId,
	
    case AServer == BServer of
	true ->
		error_logger:info_msg("Nothing is changed! Ignore this!~n"),
		do_nothing;
	false ->
		case APort == BPort andalso BIp == AIp of
			true ->
				error_logger:info_msg("check_distribution: "
						 "APort=BPort=~p; BIp=AIp=~p ~n", [APort,BIp]),
				case BNode == ANode of
					true ->
						error_logger:info_msg("Nothing has changed! Ignore this!~n"),
						do_nothing;
					false ->
						error_logger:info_msg("check_distribution:"
								 " Send rm_serverto node ~p for server ~p~n",[BNode, BServer]),
        					{?RELAY_MGR, BNode} ! {rm_server, BServer},
						case is_record(AServer, servers) of
							true ->
								AKey = AServer#servers.portIpAddr,
								F = 
									fun() ->
										[R] = mnesia:wread({servers, AKey}),
										mnesia:write(R#servers{processId = undefined})
									end,
								mnesia:transaction(F);
							_ ->
								do_nothing
						end
				end;
			false ->
				error_logger:info_msg("Port and IP pair is different!~n"),
				error_logger:info_msg("check_distribution: "
						 "Send rm_server to node ~p for server ~p~n",[BNode,BServer]),
				{?RELAY_MGR, BNode} ! {rm_server, BServer},
				case is_record(AServer, servers) of
					true ->
						AKey = AServer#servers.portIpAddr,
						F = fun() ->
							[R] = mnesia:wread({servers, AKey}),
							mnesia:write(R#servers{processId = undefined})
						end,
						mnesia:transaction(F);
					_ ->
						do_nothing
				end
		end
    end,
    check_distr(BServers, AServers).
	
	
check_distribution(BServers, AServers) ->
    error_logger:info_msg("check_distribution: BServers ~p; AServers ~p~n",[BServers, AServers]),
    lists:foreach(
	  fun(#servers{portIpAddr = {BPort,BIp}} = BServer ) ->
			  lists:foreach(
				fun(#servers{portIpAddr = {APort, AIp}} = AServer) ->
						error_logger:info_msg("check_distribution: "
								 "AServer ~p, BServer ~p~n",[AServer,BServer]),
						BNode = BServer#servers.nodeId,
						ANode =AServer#servers.nodeId,
						case AServer == BServer of
							true ->
								do_nothing;
							false ->
								case APort == BPort andalso BIp == AIp of
									true ->
										error_logger:info_msg("check_distribution: "
												 "APort=BPort=~p; BIp=AIp=~p ~n", [APort,BIp]),
										
										case BNode == ANode of
											true ->
												error_logger:info_msg("Nothing has changed! Ignore this!~n"),
												do_nothing;
											false ->
												error_logger:info_msg("check_distribution:"
														 " Send rm_serverto node ~p for server ~p~n",[BNode, BServer]),
												{?RELAY_MGR, BNode} ! {rm_server, BServer},
												case is_record(AServer, servers) of
													true ->
														AKey = AServer#servers.portIpAddr,
														F = 
															fun() ->
																	[R] = mnesia:wread({servers, AKey}),
																	mnesia:write(R#servers{processId = undefined})
															end,
														mnesia:transaction(F),
														%% Workaround for diameter instead of mnesia listener
											            %% Send Server parameters to node to connect to the server
														error_logger:info_msg("send add_server to ~p ~p ~n",[ANode,AServer]),
            											{?RELAY_MGR, ANode} ! {add_server, AServer};
													_ ->
														do_nothing
												end
										end;
									false ->
										error_logger:info_msg("Port and IP pair is different, but the node "
												 "has not changed! Ignore this!~n"),
										case BNode == ANode of
											true ->
												do_nothing;
											false ->
												error_logger:info_msg("check_distribution: "
														 "Send rm_server to node ~p for server ~p~n",[BNode,BServer]),
												{?RELAY_MGR, BNode} ! {rm_server, BServer},
												case is_record(AServer, servers) of
													true ->
														AKey = AServer#servers.portIpAddr,
														F = fun() ->
																	[R] = mnesia:wread({servers, AKey}),
																	mnesia:write(R#servers{processId = undefined})
															end,
														mnesia:transaction(F),
														%% Workaround for diameter instead of mnesia listener
											            %% Send Server parameters to node to connect to the server
														error_logger:info_msg("send add_server to ~p ~p ~n",[ANode,AServer]),
            											{?RELAY_MGR, ANode} ! {add_server, AServer};
													_ ->
														do_nothing
												end
										end
								end
						end
				end,AServers)
	  end, BServers).
