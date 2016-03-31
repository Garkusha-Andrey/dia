%% @author Aleksandra
%% @doc @todo Add description to controller_server.


-module(controller_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, check_tables/1]).
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

change_configuration(diaLocal, Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config_diaLocal, Args});
change_configuration(diaIp, Args) -> 
    gen_server:cast({global, ?MODULE}, {new_config_diaIp, Args}).
change_configuration(Args) -> 
	mnesia:system_info(),
	io:format("XKULALE2, Args ~p, Pid ~p~n",[Args, self()]),
	
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
handle_call({check_tables, Node}, _From, State) ->
	Answer = check_tables(Node),
    {reply, Answer, State};

handle_call(Call, _From, State) ->
	io:format("Call is ~p~n",[Call]),
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
handle_cast({new_config,[diameter, Enode, LocalIp, DiaInstId, MacLocalIp]}, State) ->

	io:format("I am in handle_cast for new_config for diameter!!!~n"),
	F = fun() ->
        mnesia:write(#diaConnections{node = Enode,
				     diaInstanceId = DiaInstId}),
		mnesia:write(#diaLocalConfig{diaInstanceId = DiaInstId, 
					     ipAddress = LocalIp,
					     macIpAddress = MacLocalIp})		
    end,
    mnesia:activity(transaction, F),
	{noreply, State};
handle_cast({new_config,[OVSIntIp, OVSIntMask, PublicIp, PublicMask, OVSMac, ExtGwMac]}, State) ->

	io:format("I am in handle_cast for new_config for global!!!~n"),
	
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
handle_cast(Cast, State) ->
	io:format("Cast is ~p~n",[Cast]),
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
    io:format("Befoe call rpc~n"),
	CallRess = rpc:call(Node, application, start, [mnesia]),
	io:format("CallRess ~p~n",[CallRess]),
	check_tables(Node),
	NodeL = atom_to_list(Node),
	io:format("NodeL ~p~n",[NodeL]),
	Nubmer = string:chr(NodeL, $@),
	{NodeName, _Suffix} = lists:split(Nubmer, NodeL),
	case NodeName of
		"diameter" ->
			ets:insert(diaNodes, Node),
			io:format("The new diameterNodes is ~p~n",[ets:tab2list(diaNodes)]);
		_ ->
			do_nothing
	end,
		
	
	{noreply, State};
handle_info({nodedown, Node}, State) ->
	%% do smth
    NodeL = atom_to_list(Node),
	Nubmer = string:chr(NodeL, $@),
	{NodeName, _Suffix} = lists:split(Nubmer, NodeL),
	case NodeName of
		"controller" ->
			io:format("~nNode ~w is down! The current state is ~w~n",[Node,State]),
			do_nothing;
		"diameter" ->
			io:format("~nNode ~w is down! The current state is ~w~n",[Node,State]),
			controller_lib:delete(Node),
			SessionPid = controller_lib:get_session_pid(Node),
			{SessionPid, Node} ! terminate,			
			routing:update()
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
	io:format("I am in check_table~n"),
	case mnesia:change_config(extra_db_nodes, [Node]) of
		{ok, [Node]} ->
			io:format("change_config is ok~n"),
			case catch mnesia:table_info(diaConnections, attributes) of
				{'EXIT', _} ->
					io:format("Exit for diaConfig~n"),
					mnesia:create_table(diaConnections,
                        [{record_name, diaConnections},
						 {attributes, record_info(fields, diaConnections)},
                         {ram_copies, Node}]),
					mnesia:add_table_copy(diaConnections, Node, ram_copies);
				_ ->
					io:format("Tabinfo for diaconfig~n"),
					mnesia:add_table_copy(diaConnections, Node, ram_copies)
			end,
			case catch mnesia:table_info(diaLocalConfig, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(diaLocalConfig,
										[{attributes, record_info(fields, diaLocalConfig)},
										 {ram_copies,  Node}]);
				_ ->
					io:format("Tabinfo for diaLocalConfig~n"),
					mnesia:add_table_copy(diaLocalConfig, Node, ram_copies)
			end,
			case catch mnesia:table_info(globalData, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(globalData,
										[{attributes, record_info(fields, globalData)},
										 {ram_copies,  Node}]);
				_ ->
					mnesia:add_table_copy(globalData, Node, ram_copies)
			end,
			case catch mnesia:table_info(instanceWeight, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(instanceWeight,
										[{attributes, record_info(fields, instanceWeight)},
										 {ram_copies,  Node}]);
				_ ->
					io:format("Tabinfo for instanceWeight~n"),
					mnesia:add_table_copy(instanceWeight, Node, ram_copies)
			end,
			case catch mnesia:table_info(servers, attributes) of
				{'EXIT', _} ->
					mnesia:create_table(servers,
										[{attributes, record_info(fields, servers)},
										 {ram_copies,  Node}]);
				_ ->
					io:format("Tabinfo for instanceWeight~n"),
					mnesia:add_table_copy(servers, Node, ram_copies)
			end;
		_ ->
			do_nothing
	end.
	
    





			


