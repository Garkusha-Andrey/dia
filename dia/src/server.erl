%% Usage:
%%   diameter:start().
%%   server:start(s1, 'ex.com').
%%   server:listen(s1, {tcp, loopback, 3911}).
%%
%%   server:deploy([s1, 'ex.ru', {127,0,0,1}, 3911, server]).
%%

-module(server).

-export([start/2,    %% start a service
         start/3,    %%
         listen/2,   %% add a listening transport
         stop/1,
		 deploy/1]).   %% stop a service

-define(DEF_SVC_NAME, ?MODULE).
-define(SERVER_CALLBACK_MOD, server_cb).%% define server callback
-define(IRELAY_CALLBACK_MOD, i_relay_cb).%% define server callback
-define(L, atom_to_list).
-define(DEFAULT_PORT, 3911).


-define(SERVICE(HostName, Realm), [{'Origin-Host', ?L(HostName) ++ "." ++ ?L(Realm)},
                        {'Origin-Realm', ?L(Realm)},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [0]},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, ?SERVER_CALLBACK_MOD}]}]).


%% deploy([<Name>, <Ralm>, <IP>, <Port>])
%% server:deploy(['s1','ex.ru','127.0.0.1','3911']). - from Erlang mashine
%% erl -s server deploy 's1' 'ex.com' "127.0.0.1" 3911 - from bash
deploy(T) ->
    Name = lists:nth(1, T),
	Realm = lists:nth(2, T),
	{ok, LIp} = inet_parse:address(atom_to_list(lists:nth(3, T))),
	Port  = list_to_integer(atom_to_list(lists:nth(4, T))),

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
%% XANDRGA: Use tcp: listen(server1, 1234)
listen(Name, Port)
  when is_integer(Port) ->
    node:listen(Name, {tcp, loopback, Port});

listen(Name, T) ->
    node:listen(Name, T).

%% stop/1

stop(Name) ->
    node:stop(Name).
