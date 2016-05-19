%% @author Aleksandra Kulakova
%% @doc @todo Add description to controller_app.


-module(controller_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([install/1,
		 change_configuration/1,
		 parse_service_config/1,
		 clodify_done/0,
		 clients_updated/0]).

-include("controller_app.hrl").

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @doc Must be called from script or erlang shell
install(Nodes) ->
	io:format("~nI am in install() for nodes ~p~n",[Nodes]),
	%%Start needed applications on all nodes:
	rpc:multicall(Nodes, application, start, [mnesia]),
	rpc:multicall(Nodes, application, start, [inets]),
	
	%%Start needed ETS tables:
	ets:new(diaNodes, [set, named_table, {keypos, 2}, public]),
	ets:new(sd_table, [set, named_table, {keypos, 2}, public]),
	ets:new(tbd_table,[set, named_table, {keypos, 2}, public]),

	%%Distribute all mnesia tables:
	MnesiaConfig = mnesia:change_config(extra_db_nodes, Nodes),
	io:format("MnesiaConfig is ~p~n",[MnesiaConfig]),
	case mnesia:change_config(extra_db_nodes, Nodes) of
		{ok, _ENodes} ->
			io:format("XKULALE: Nodes ~p~n",[Nodes]),
			case catch mnesia:table_info(diaConnections, attributes) of
				{'EXIT', _} ->
					Result = mnesia:create_table(diaConnections,
                        [{record_name, diaConnections},
						 {attributes, record_info(fields, diaConnections)},
                         {ram_copies, Nodes}]),
					io:format("Result is ~p~n",[Result]);
				TabInfo1 ->
					io:format("TabInfo is ~p~n",[TabInfo1]),
					mnesia:add_table_copy(diaConnections, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(diaLocalConfig, attributes) of
				{'EXIT', _} ->
					Result2 = mnesia:create_table(diaLocalConfig,
										[{record_name, diaLocalConfig},
										 {attributes, record_info(fields, diaLocalConfig)},
										 {ram_copies, Nodes}]),
					io:format("Result is ~p~n",[Result2]);
				TabInfo3 ->
					io:format("TabInfo is ~p~n",[TabInfo3]),
					mnesia:add_table_copy(diaLocalConfig, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(globalData, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(globalData,
										[{record_name, globalData},
										 {attributes, record_info(fields, globalData)},
										 {ram_copies, Nodes}]);
				TabInfo4 ->
					io:format("TabInfo is ~p~n",[TabInfo4]),
					mnesia:add_table_copy(globalData, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(instanceWeight, attributes) of
				{'EXIT', _} ->
					Result3 = mnesia:create_table(instanceWeight,
										[{record_name, instanceWeight},
										 {attributes, record_info(fields, instanceWeight)},
										 {ram_copies, Nodes}]),
					io:format("Result is ~p~n",[Result3]);
				TabInfo5 ->
					io:format("TabInfo is ~p~n",[TabInfo5]),
					mnesia:add_table_copy(instanceWeight, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(servers, attributes) of
				{'EXIT', _} ->
					Result4 = mnesia:create_table(servers,
										[{record_name, servers},
										 {attributes, record_info(fields, servers)},
										 {ram_copies, Nodes}]),
					io:format("Result is ~p~n",[Result4]);
				TabInfo6 ->
					io:format("TabInfo is ~p~n",[TabInfo6]),
					mnesia:add_table_copy(servers, Nodes, ram_copies)
			end,
		case catch mnesia:table_info(clients, attributes) of
				{'EXIT', _} ->
					Result5 = mnesia:create_table(clients,
										[{record_name, clients},
										 {attributes, record_info(fields, clients)},
										 {ram_copies, Nodes}]),
					io:format("Result5 is ~p~n",[Result5]);
				TabInfo7 ->
					io:format("TabInfo7 is ~p~n",[TabInfo7]),
					mnesia:add_table_copy(clients, Nodes, ram_copies)
			end;
		_ ->
			do_nothing
	end.


%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
 start({failover, _Node}, _Args) ->
	case application:get_application(mnesia) of
		undefined ->
			application:start(mnesia);
		{ok, mnesia} ->
			ok
	end,
	Nodes = [node()| nodes()],
	install(Nodes),
	controller_lib:initialize_routing(10),
	wait_for_tables(),	
    controller_sub:start_link();
start(normal, []) ->
	io:format("~n Start normally! ~n"),
	Nodes = [node()| nodes()],
	install(Nodes),
	wait_for_tables(),
    controller_sub:start_link();
start({takeover, OtherNode}, []) ->
	io:format("Takeover for the node ~p is started!~n",[OtherNode]),
	case application:get_application(mnesia) of
		undefined ->
			application:start(mnesia);
		{ok, mnesia} ->
			ok
	end,
	Nodes = [node()| nodes()],
	install(Nodes),
	wait_for_tables(),
	controller_lib:initialize_routing(10),
    controller_sub:start_link().

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
	%Nodes = [node() | nodes()],
	%mnesia:delete_schema(Nodes),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
%%Configure instances:
change_configuration(Args) ->
	timer:sleep(10000),
	controller_server:change_configuration(Args).

%% ====================================================================
%%Wait until the mnesia is connected.
%% ====================================================================
wait_for_tables() ->
	mnesia:wait_for_tables([diaConnections,
							diaLocalConfig,
							globalData,
							instanceWeight,
							servers,
							clients], 
						   15000).

%% ====================================================================
%%Parse configuration of servers.
%% ====================================================================
parse_service_config(InputFileName) ->
	Device = open_file(InputFileName, read),
    read_lines(Device, []),
    close_file(Device).

%% ====================================================================
%%Helper functions for parse_service_config/1.
%% ====================================================================
open_file(FileName, Mode) ->
    {ok, Device} = file:open(FileName, [Mode, binary]),
    Device.

close_file(Device) ->
    ok = file:close(Device).

read_lines(Device, L) ->
    case io:get_line(Device, L) of
        eof ->
            lists:reverse(L);
        String ->
			SL = binary_to_list(String),
			case lists:member($#, SL) of
				true ->
					%%comment line, skip it:
					do_nothing;
				false ->
					{ok, ItemTokens, _} = erl_scan:string(SL ++ "."),
					{ok, {RealmId, RealmHost, IpAddress, Port}} = erl_parse:parse_term(ItemTokens),
					F = fun() ->
								mnesia:write(#servers{portIpAddr = {Port, IpAddress},
													  realmId = RealmId,
													  realmHost = RealmHost})
						end,
					mnesia:activity(transaction, F);
				_ ->
					do_nothing
			end,
			Bin = read_lines(Device, [SL | L]),
			Bin
    end.

%% ====================================================================
%% Calls from cloudify to start parse the current server configuration
%% and distribute the diameter instances per server.
%% ====================================================================
clodify_done() ->
	parse_service_config("../src/service_config"),
	controller_server:initial_distribution().
  
%% ====================================================================
%% Calls by diameter application to "tell" Routing that clients 
%% configuration has been updated.
%% ====================================================================
clients_updated() ->
	controller_server:clients_updated().


