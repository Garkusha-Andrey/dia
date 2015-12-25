%% @author Aleksandra
%% @doc @todo Add description to boot.


-module(boot).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
	case application:get_application(controller_app) of
		undefined ->
			application:start(controller_app);
		{ok _Value} ->
            do_nothing
    end.

 

