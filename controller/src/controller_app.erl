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
	io:format("~nI am in install() for nodes ~p~n",[Nodes]),
	%Result = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	
	%io:format("~nResult is ~p~n",[Result]),
    Result1 = mnesia:create_table(diaConfig,
                        [{record_name, diaConfig},
						 {attributes, record_info(fields, diaConfig)},
                         {ram_copies, Nodes}]),
	io:format("~nResult1 is ~w~n",[Result1]),
    Result2 = mnesia:create_table(diaIpAddress,
                        [
						 %{attributes, set},
                         {ram_copies, Nodes}]),
	io:format("~nResult2 is ~w~n",[Result2]),
    Result3 = mnesia:create_table(diaLocalIpAddress,
                        [
						 %{attributes, record_info(fields, diaLocalIpAddress)},
                         {ram_copies,  Nodes}]),
	mnesia:change_config(extra_db_nodes, Nodes),
                         %{local_content, true}]),
	io:format("~nResult3 is ~w~n",[Result3]),
	ok.

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
	case application:get_application(mnesia) of
		undefined ->
			application:start(mnesia);
		{ok, mnesia} ->
			ok
	end,
	io:format("~n Before install!!! ~n"),
	Nodes = [node()| nodes()],
	install(Nodes),
	wait_for_tables(),
	lists:foreach(fun(Node) ->
						  mnesia:add_table_copy(diaConfig, Node,
											   ram_copies)
				  end,
				  Nodes),
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
change_configuration([PeerId, RemotePeerIp, DiaInstanceId]) ->
	
	F = fun() ->
        mnesia:write(#diaConfig{peerId         = PeerId,
								remotePeerIp  = RemotePeerIp,
								diaInstanceId = DiaInstanceId})
    end,
    mnesia:activity(transaction, F).
	%controller_server:change_configuration(Arg).

wait_for_tables() ->
mnesia:wait_for_tables([diaConfig,
						diaIpAddress,
						diaLocalIpAddress], 
					   15000).

