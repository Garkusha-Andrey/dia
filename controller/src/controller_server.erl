%% @author Aleksandra
%% @doc @todo Add description to controller_server.


-module(controller_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, check_tables/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2,initial_distribution/0,
		 get_nodes_from_servers/1,get_nodes/0, clients_updated/0]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([change_configuration/1,
		 change_configuration/2]).
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
	net_kernel:monitor_nodes(true),
    {ok, []}.


start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

change_configuration(diaLocal, Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config_diaLocal, Args});
change_configuration(diaIp, Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config_diaIp, Args}).
change_configuration(Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config, Args}).

initial_distribution() ->
	gen_server:call({global, ?MODULE}, initial_distribution).

clients_updated() ->
	gen_server:cast({global, ?MODULE}, clients_updated).

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
handle_call(question, _From, State) ->
    {ok, Answers} = application:get_env(controller_app, answers),
    Answer = element(random:uniform(tuple_size(Answers)), Answers),
    {reply, Answer, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({check_tables, Node}, _From, State) ->
	Answer = check_tables(Node),
    {reply, Answer, State};
handle_call(initial_distribution, _From, State) ->
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
handle_cast({new_config,[diameter, Enode, LocalIp, LocalMac]}, State) ->

	F = fun() ->
		mnesia:write(#diaConnections{nodeId = Enode}),
		mnesia:write(#diaLocalConfig{nodeId = Enode, 
					     ipAddress  = LocalIp,
					     macAddress = atom_to_list(LocalMac)})
    end,
    mnesia:activity(transaction, F),
	{noreply, State};
handle_cast({new_config,[OVSIntIp, OVSIntMask, PublicIp, PublicMask, OVSMac, ExtGwMac]}, State) ->
	
	F = fun() ->
		mnesia:write(#globalData{ovsIpMask = {atom_to_list(OVSIntIp),
						      atom_to_list(OVSIntMask)},
					 publicIpMask = {atom_to_list(PublicIp),
							 atom_to_list(PublicMask)},
					 ovsMac   = atom_to_list(OVSMac),
					 extGwMac = atom_to_list(ExtGwMac)})
		
    end,

    mnesia:activity(transaction, F),
	routing:init(),
	{noreply, State};
handle_cast({new_config_diaLocal,IpAddress}, State) ->
	F = fun() ->
        mnesia:write(#diaLocalConfig{ipAddress = IpAddress})
    end,
    mnesia:activity(transaction, F),
	{noreply, State};

handle_cast(clients_updated, State) ->
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
	rpc:call(Node, application, start, [mnesia]),
	rpc:call(Node, application, start, [inets]),
	check_tables(Node),
	NodeL = atom_to_list(Node),
	case string:str(NodeL, "diameter") of
		1 ->
			ets:insert(diaNodes, {node, Node});
		0 ->
			do_nothing
	end,
	
	{noreply, State};
handle_info({nodedown, Node}, State) ->
	%% do smth
    NodeL = atom_to_list(Node),
	case string:str(NodeL, "diameter") of
		1 ->
			ets:delete(diaNodes, Node),
			BServers = controller_lib:list_servers(),
			controller_lib:delete(Node),
			redistribute_servers(),
			AServers = controller_lib:list_servers(),
			check_distribution(BServers, AServers),
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
	
redistribute_servers() ->
	Servers = get_not_distributed_servers(),
	Nodes = get_nodes(),
	case Nodes of
		[] ->
			io:format("No any DIA node. Nothing to distribute!~n");
		_ ->
			NodeSumL = get_nodes_from_servers(Nodes),
			NewNodeSumL = lists:keysort(2,lists:keysort(1, NodeSumL)),
			NewNodes = [K || {K, _} <- NewNodeSumL],
			distribute_server_per_node(Servers, NewNodes)
end.

get_nodes_from_servers(Nodes) ->
	lists:foldr(fun(Node, Acc) ->
						F = fun() -> 
									Keys = mnesia:all_keys(servers),
									Sum = lists:foldl(fun(Key, Acc1) ->
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
								error_logger:error_report("ERROR: Immpossible to get diaConnections due to ~p",[Reason]),
								Acc;
							{atomic, ResultOfFun} ->
								[ResultOfFun | Acc]
						end
				end,
				[],
				Nodes).
	
									  
initial_servers_distribution() ->
	DistrServers = controller_lib:list_servers(),
	Servers = case DistrServers of
				  [] ->
					  %%Servers do not exist or no any servers distributed:
	                  get_not_distributed_servers();
				_ ->
					%%Re-distribution of all servers:
					DistrServers
			  end,
	Nodes = get_nodes(),
	case Nodes of
		[] ->
			error_logger:error_report("Servers could not be distributed. No any DIA nodes exist!");
		_ ->
			distribute_server_per_node(Servers, Nodes)
	end,
	RedistrServers = controller_lib:list_servers(),
	check_distribution(DistrServers, RedistrServers),
	routing:update().


distribute_server_per_node([], _Nodes) ->
	ok;
distribute_server_per_node([Server | _Servers] = TotServers, Nodes) ->
	RealmId = Server#servers.realmId,
	TmpServers = get_all_servers_realm(RealmId),
	NewNodes = distribute_servers(TmpServers, Nodes),
	NewServers = TotServers -- TmpServers,
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

store_nodeId(Server, NodeId) ->
	Key = Server#servers.portIpAddr,
	F = fun() ->
				[R] = mnesia:wread({servers, Key}),
				mnesia:write(R#servers{nodeId = NodeId})
		end,
	Result = mnesia:transaction(F),
	case Result of
{aborted, Reason} ->
	error_logger:error_report("ERROR: Immpossible to"
			 " get servers due to ~p~n",[Reason]);
		{atomic, Table} ->
			Table

	end.

get_all_servers_realm(RealmId) ->
	F = fun() ->
				PortIp = mnesia:all_keys(servers),
				RecordList  = lists:foldl(fun(Elem, Acc) ->
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
			error_logger:error_report("ERROR: Immpossible to get servers due to ~p",[Reason]);
		{atomic, Servers} ->
			Servers
	end.
get_not_distributed_servers() ->
	F = fun() ->
				PortIp = mnesia:all_keys(servers),
				RecordList  = lists:foldl(fun(Elem, Acc) ->
												  Acc ++ mnesia:read(servers, Elem)
										  end,
										  [],
										  PortIp),
				NotDistRecordList =lists:filter(fun(#servers{nodeId = NodeId}) ->
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
			error_logger:error_report("ERROR: Immpossible to get servers due to ~p",[Reason]);
		{atomic, Servers} ->
			Servers
	end.

check_distribution(BServers, AServers) ->
	lists:foreach(fun(#servers{portIpAddr = {Port, Ip}} = BServer ) ->
						  AServer = lists:keyfind({Port, Ip}, 2, AServers),
						  case AServer == BServer of
							  true ->
								  do_nothing;
							  false ->
								  Node = BServer#servers.nodeId,
								  Pid = BServer#servers.processId,
								  case Pid of
									  undefined ->
										  do_nothing;
									  _ ->
										  {Pid, Node} ! terminate,
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
						  end
				  end,
				  BServers).


			


