%% @author Aleksandra
%% @doc @todo Add description to boot.


-module(boot).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
		 start/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
io:format("I am in boot:start()~n"),
	case application:get_application(controller_app) of
		undefined ->
			application:start(controller_app);
		{ok, _Value} ->
            do_nothing
    end.

start([Node]) ->
	case net_adm:ping(Node) of
		pong ->
			start();
		_ ->
			error_logger:error_report("The node ~p could not be connected to node ~p!~n",[node(), Node])
	end.	

 
