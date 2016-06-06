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
	error_logger:info_msg("An initialization of the nodes ~p are started ~n",[Nodes]),
    %%Start needed applications on all nodes:
    rpc:multicall(Nodes, application, start, [mnesia]),
    rpc:multicall(Nodes, application, start, [inets]),

    %%Start needed ETS tables:
    case ets:info(diaNodes) of
		undefined ->
			ets:new(diaNodes, [set, named_table, {keypos, 2}, public]);
		_ -> do_nothing
	end,
	case ets:info(sd_table) of
		undefined ->
			ets:new(sd_table, [set, named_table, {keypos, 2}, public]);
		_ -> do_nothing
	end,
	case ets:info(tbd_table) of
		undefined ->
			ets:new(tbd_table, [set, named_table, {keypos, 2}, public]);
		_ -> do_nothing
	end,

	%%Distribute all mnesia tables:
	case mnesia:change_config(extra_db_nodes, Nodes) of
		{ok, _ENodes} ->
			error_logger:info_msg("Mnesia is started on the nodes ~p~n",[Nodes]),
			case catch mnesia:table_info(diaConnections, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaConnections,
										[{record_name, diaConnections},
										 {attributes, record_info(fields, diaConnections)},
										 {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(diaConnections, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(diaLocalConfig, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaLocalConfig,
										[{record_name, diaLocalConfig},
										 {attributes, record_info(fields, diaLocalConfig)},
										 {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(diaLocalConfig, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(globalData, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(globalData,
										[{record_name, globalData},
										 {attributes, record_info(fields, globalData)},
										 {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(globalData, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(instanceWeight, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(instanceWeight,
										[{record_name, instanceWeight},
										 {attributes, record_info(fields, instanceWeight)},
										 {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(instanceWeight, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(servers, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(servers,
										[{record_name, servers},
										 {attributes, record_info(fields, servers)},
										 {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(servers, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(clients, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(clients,
										[{record_name, clients},
										 {attributes, record_info(fields, clients)},
                                         {ram_copies, Nodes}]);
				_ ->
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
 %start({failover, _Node}, _Args) ->
%       case application:get_application(mnesia) of
%               undefined ->
%                       application:start(mnesia);
%               {ok, mnesia} ->
%                       ok
%       end,
%       Nodes = [node()| nodes()],
%       install(Nodes),
%       controller_lib:initialize_routing(20),
%       wait_for_tables(),
 %   controller_sub:start_link();
start(normal, []) ->
    error_logger:info_msg("Erlang controller application start normally! ~n"),
    Nodes = [node()| nodes()],
    install(Nodes),
    wait_for_tables(),
    controller_lib:initialize_routing(20),
    controller_sub:start_link();
start({takeover, OtherNode}, []) ->
    error_logger:info_msg("Erlang controller application takeover for "
                          "the node ~p is started!~n",[OtherNode]),
    case application:get_application(mnesia) of
	undefined ->
		application:start(mnesia);
	{ok, mnesia} ->
		ok
    end,
    Nodes = [node()| nodes()],
    install(Nodes),
    wait_for_tables(),
    controller_lib:initialize_routing(20),
    error_logger:info_msg("Controller supervisor start begins !~n"),
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
    error_logger:info_msg("The configuration with arguments ~p begins!~n",[Args]),
    timer:sleep(10000),
	
    controller_server:change_configuration(Args).

%% ====================================================================
%%Wait until the mnesia is connected.
%% ====================================================================
wait_for_tables() ->
    error_logger:info_msg("Wainting for mnesia tables load... ~n"),
    mnesia:wait_for_tables([diaConnections,
                            diaLocalConfig,
			    globalData,
			    instanceWeight,
			    servers,
			    clients],
			    15000),
    error_logger:info_msg("All mnesia tables loaded... ~n").

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
    case controller_lib:check_mnnesia_entries(servers) of
	'$end_of_table' ->
		error_logger:info_msg("Parse service_config is started!~n"),
		parse_service_config("../src/service_config"),
		error_logger:info_msg("Parse service_config is finished!~n"),
		error_logger:info_msg("Send initial distribution request to controller_server~n");
	_ ->
		do_nothing
    end,
    controller_server:initial_distribution().

%% ====================================================================
%% Calls by diameter application to "tell" Routing that clients
%% configuration has been updated.
%% ====================================================================
clients_updated() ->
    controller_server:clients_updated().
