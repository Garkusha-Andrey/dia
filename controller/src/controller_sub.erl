%% @author Aleksandra
%% @doc @todo Add description to controller_sub.


-module(controller_sub).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    {ok, {{one_for_one, 1, 10},
          [{controller_sub,
            {controller_server, start_link, []},
            permanent,
            5000,
            worker,
            [controller_server]
          }]}}.

start_link() ->
    supervisor:start_link({global,?MODULE}, ?MODULE, []).
    

%% ====================================================================
%% Internal functions
%% ====================================================================



