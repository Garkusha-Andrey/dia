%% @author Aleksandra
%% @doc @todo Add description to controller_server.


-module(controller_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, ask/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
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

ask(Question) -> % the question doesn't matter!
    gen_server:cast({global, ?MODULE}, {new_config, Question}).
	%gen_server:call({global, ?MODULE}, question).


change_configuration(diaLocal, Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config_diaLocal, Args});
change_configuration(diaIp, Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config_diaIp, Args}).
change_configuration(Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config, Args}).


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

handle_cast({new_config,{PeerId, RemotePeerIp, DiaInstanceId}}, State) ->
	io:format("I am in handle_cast for new_config!!!~n"),
	F = fun() ->
        mnesia:write(diaConfig, #diaConfig{peerId         = PeerId,
										   remotePeerIp  = RemotePeerIp,
										   diaInstanceId = DiaInstanceId})
    end,
    mnesia:activity(transaction, F),
	{noreply, State};
handle_cast({new_config_diaLocal,IpAddress}, State) ->
	F = fun() ->
        mnesia:write(#diaLocalIpAddress{ipAddress = IpAddress})
    end,
    mnesia:activity(transaction, F),
	{noreply, State};
handle_cast({new_config_diaIp,IpAddress}, State) ->
	F = fun() ->
        mnesia:write(#diaIpAddress{ipAddress = IpAddress})
    end,
    mnesia:activity(transaction, F),
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
	%% do smth
    io:format("~nNode ~w is up!~n~n",[Node]),
	rpc:call(Node, application, start, [mnesia]),
	check_tables(Node),
	{noreply, State};
handle_info({nodedown, Node}, State) ->
	%% do smth
    io:format("~nNode ~w is down! The current state is ~w~n",[Node,State]),
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
			case catch mnesia:table_info(diaConfig, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaConfig,
                        [{record_name, diaConfig},
						 {attributes, record_info(fields, diaConfig)},
                         {ram_copies, Node}]);
				_ ->
					mnesia:add_table_copy(diaConfig, Node, ram_copies)
			end,
			case catch mnesia:table_info(diaIpAddress, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaIpAddress,
                        [
						 %{attributes, set},
                         {ram_copies, Node}]);
				_ ->
					mnesia:add_table_copy(diaIpAddress, Node, ram_copies)
			end,
			case catch mnesia:table_info(diaLocalIpAddress, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaLocalIpAddress,
										[{attributes, record_info(fields, diaLocalIpAddress)},
										 {ram_copies,  Node}]);
				_ ->
					mnesia:add_table_copy(diaLocalIpAddress, Node, ram_copies)
			end;
		_ ->
			do_nothing
	end.
	
    





			



