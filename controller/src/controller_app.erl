%% @author Aleksandra Kulakova
%% @doc @todo Add description to controller_app.


-module(controller_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ask/1, install/1,
		 change_configuration/1,
		 change_configuration/2]).

-include("controller_app.hrl").

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @doc Must be called from script or erlang shell
install(Nodes) ->
	%erl -sname b -kernel distributed \[\{controller_app,5000,\[\'a@alexa-Aspire-V5-572PG\',\{\'b@alexa-Aspire-V5-572PG\',\'c@alexa-Aspire-V5-572PG\'\}\]\}\] -kernel sync_nodes_mandatory \[\'a@alexa-Aspire-V5-572PG\',\'c@alexa-Aspire-V5-572PG\'\]
	io:format("~nI am in install() for nodes ~p~n",[Nodes]),
	%Result = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	%rpc:multicall(Nodes, application, start, [controller_app]),

	case mnesia:change_config(extra_db_nodes, Nodes) of
		{ok, Nodes} ->
			case catch mnesia:table_info(diaConfig, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaConfig,
                        [{record_name, diaConfig},
						 {attributes, record_info(fields, diaConfig)},
                         {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(diaConfig, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(diaIpAddress, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaIpAddress,
                        [{record_name, diaIpAddress},
						 {attributes, record_info(fields, diaIpAddress)},
                         {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(diaIpAddress, Nodes, ram_copies)
			end,
			case catch mnesia:table_info(diaLocalIpAddress, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaLocalIpAddress,
										[{record_name, diaLocalIpAddress},
										 {attributes, record_info(fields, diaLocalIpAddress)},
										 {ram_copies, Nodes}]);
				_ ->
					mnesia:add_table_copy(diaLocalIpAddress, Nodes, ram_copies)
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
%% start({failover, Node}, Args) is only called
%% when a start_phase key is defined.
start(normal, []) ->
	%case application:get_application(mnesia) of
	%	undefined ->
	%		application:start(mnesia);
	%	{ok, mnesia} ->
	%		ok
	%end,
	io:format("~n Before install!!! ~n"),
	Nodes = [node()| nodes()],
	install(Nodes),
	wait_for_tables(),
    controller_sub:start_link();
start({takeover, _OtherNode}, []) ->
	case application:get_application(mnesia) of
		undefined ->
			application:start(mnesia);
		{ok, mnesia} ->
			ok
	end,
	Nodes = [node()| nodes()],
	install(Nodes),
	wait_for_tables(),
    controller_sub:start_link().

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
	io:format("I am in stop~n"),
	%Nodes = [node() | nodes()],
	%mnesia:delete_schema(Nodes),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
ask(Question) ->
    controller_server:ask(Question).

change_configuration(diaLocal, Arg) ->	
	controller_server:change_configuration(diaLocal, Arg);
change_configuration(diaIp, Arg) ->
	controller_server:change_configuration(diaIp, Arg).
change_configuration(Args) ->
	io:format("XKULALE~n"),
	controller_server:change_configuration(Args).

wait_for_tables() ->
mnesia:wait_for_tables([diaConfig,
						diaIpAddress,
						diaLocalIpAddress,
						globalData,
						instanceWeight], 
					   15000).

