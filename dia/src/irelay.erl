%% @author agarkush
-module(irelay).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-include_lib("dia_relay_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([deploy/1,
		stop/1]).

-export([init/1]).

-define(DEF_SVC_NAME, ?MODULE).
-define(IRELAY_CALLBACK_MOD, i_relay_cb).
-define(L, atom_to_list).
-define(DEFAULT_PORT, 3911).


-define(SERVICE(HostName, Realm), [{'Origin-Host', ?L(HostName) ++ "." ++ ?L(Realm)},
                        {'Origin-Realm', ?L(Realm)},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [0]},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, ?IRELAY_CALLBACK_MOD}]}]).

-define(LISTENER_PROCESS, irelay_l).

init(NodeName) ->
	case ets:info(next_hope) of
		undefined ->
			ets:new(next_hope, [set, named_table, public]);
		_ -> do_nothing
	end,
	ets:insert(next_hope, {active, NodeName, ?LISTENER_PROCESS}).

%% deploy/1 ([<Name>, <Ralm>, <IP>, <Port>])
%% irelay:deploy(['ir1','ex.ru','127.0.0.1','3911']). - from Erlang mashine
%% erl -s irelay deploy 'ir1' 'ex.com' "127.0.0.1" 3911 - from bash
deploy(T) ->
	{ok, Log} = file:open("irelay.log", [append]),
	erlang:group_leader(Log, self()),
	
    Name  = lists:nth(1, T),
	Realm = lists:nth(2, T),
	{ok, LIp} = inet_parse:address(atom_to_list(lists:nth(3, T))),
	Port  = list_to_integer(atom_to_list(lists:nth(4, T))),

    diameter:start(),
	
	start(Name, Realm),
    listen(Name, {nfv_tcp, LIp, Port}).

%% start/2
start(Name, Realm)
  when is_atom(Name) ->
    start(Name, Realm, []).

%% start/3
start(Name, Realm, Opts) ->
    node:start(Name, Opts ++ [T || {K,_} = T <- ?SERVICE(Name, Realm),
                                   false == lists:keymember(K, 1, Opts)]).

%% listen/2
listen(Name, T) ->
    node:listen(Name, T).

%% stop/1

stop(Name) ->
    node:stop(Name).


%% ====================================================================
%% Internal functions
%% ====================================================================
