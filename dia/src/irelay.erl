%% @author agarkush
-module(irelay).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%export([deploy/1,
%		start/2,    %% start a service
%        start/3,    %%
%        listen/2,   %% add a listening transport
%        stop/1]).   %% stop a service

-export([deploy/1,
		stop/1]).

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

%% deploy/1
%% deploy([<Name>, <Ralm>, <IP>, <Port>])
%% deploy([or1, 'ex.ru', {127,0,0,1}, 3911]).
deploy(T) ->
    Name = lists:nth(1, T),
	Realm = lists:nth(2, T),
	LIp = lists:nth(3, T),
	Port = lists:nth(4, T),

    diameter:start(),
	
	start(Name, Realm),
    listen(Name, {tcp, LIp, Port}).

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

