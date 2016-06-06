%% @author Aleksandra
%% @doc @todo Add description to boot.


-module(boot).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,
		 start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
error_logger:info_msg("I am in boot:start()~n"),
	case application:get_application(controller_app) of
		undefined ->
			io:format("Controller App is NOT started!~n"),
		        application:start(inets),
			application:start(controller_app);
		{ok, _Value} ->
			error_logger:info_msg("Controller App is started!~n")
    end.
start([REnode]) ->
	error_logger:error_report("The node ~p  connects to node ~p!~n",[node(), REnode]),
	case net_adm:ping(REnode) of
		pong ->
			ok;
		_ ->
			error_logger:error_report("The node ~p could not be connected to node ~p!~n",[node(), REnode])
	end.	