%% Usage:
%%   diameter:start().
%%   server:start(s1, 'ex.com').
%%   server:listen(s1, {tcp, loopback, 3911}).
%%
%%   server:deploy()
%%

-module(server).

-export([start/3,    %% start a service
         start/4,    %%
         listen/2,   %% add a listening transport
         stop/1,
		 deploy/1]).   %% stop a service

-define(DEF_SVC_NAME, ?MODULE).
-define(CALLBACK_MOD, server_cb).%% define server callback
-define(L, atom_to_list).
-define(DEFAULT_PORT, 3911).


%% The service configuration. In a server supporting multiple Diameter
%% applications each application may have its own, although they could all
%% be configured with a common callback module.
-define(SERVICE(HostName, Realm), [{'Origin-Host', ?L(HostName) ++ "." ++ ?L(Realm)},
                        {'Origin-Realm', ?L(Realm)},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [0]},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, ?CALLBACK_MOD}]}]).

-define(SERVICE(HostName, Realm, Callback_mod), [{'Origin-Host', ?L(HostName) ++ "." ++ ?L(Realm)},
                        {'Origin-Realm', ?L(Realm)},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [0]},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, Callback}]}]).

%% deploy/1
%% deploy([<Name>, <Ralm>, <IP>, <Port>])
%% deploy([s1, 'ex.ru', {127,0,0,1}, 3911]).
deploy(T) ->
    Name = lists:nth(1, T),
	Realm = lists:nth(2, T),
	LIp = lists:nth(3, T),
	Port = lists:nth(4, T),

    diameter:start(),
    start(Name, Realm, base),
    listen(Name, {tcp, LIp, Port})
	.

%% start/3
start(Name, Realm, base)
  when is_atom(Name) ->
    start(Name, Realm, ?CALLBACK_MOD, []);
	
start(Name, Realm, relay)
  when is_atom(Name) ->
    start(Name, Realm, ?CALLBACK_MOD, []).

%% start/4
start(Name, Realm, Callback, Opts) ->
    node:start(Name, Opts ++ [T || {K,_} = T <- ?SERVICE(Name, Realm, Callback),
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
