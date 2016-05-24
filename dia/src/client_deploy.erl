-module(client_deploy).

-export([start/2,
		 start_traffic/0,
		 add_clients/4]).

-define(RIP, remote_ip).
%%-define(LOCAL_IP, '127.0.0.1').
%%-define(REMOTE_IP, '127.0.0.1').
-define(SERVER_PORT, '3911').
-define(TRAFFIC_INTERVAL, 2000).

start(LocalIP, RemoteIp) ->
	io:format("Start of Erlang client_deploy~n"),
	add_clients('c', LocalIP, RemoteIp, 5).

add_clients(Pattern, LocalIP, RemoteIp, Count) ->
	io:format("Count: ~p~n", [Count]),
	add_clients_pattern(Pattern, 'ex.com', LocalIP, RemoteIp, ?SERVER_PORT, 0, Count).

start_traffic() ->
	continue_call_stub().

continue_call_stub() ->
	continues_call(c_1, "s1", "ex1.com", ?TRAFFIC_INTERVAL),
	continues_call(c_2, "s2", "ex2.com", ?TRAFFIC_INTERVAL),
	continues_call(c_3, "s3", "ex3.com", ?TRAFFIC_INTERVAL),
	continues_call(c_4, "s4", "ex4.com", ?TRAFFIC_INTERVAL).

continues_call(Name, Realm, Host, IntervalMS) ->
	client:call(Name, user, Realm, Host),
	timer:sleep(IntervalMS)
	.

%% 'c1','ex.ru','127.0.0.1','127.0.0.1','3911'
add_clients_pattern(NamePattern, Realm, LocalIP, RemoteIP, Port, To, To) ->
	ServiceName = list_to_atom(lists:concat([NamePattern, "_", To])),
	client:deploy([ServiceName, Realm, LocalIP, RemoteIP, Port]);

add_clients_pattern(NamePattern, Realm, LocalIP, RemoteIP, Port, From, To) ->
	ServiceName = list_to_atom(lists:concat([NamePattern, "_", From])),
	client:deploy([ServiceName, Realm, LocalIP, RemoteIP, Port]),
	add_clients_pattern(NamePattern, Realm, LocalIP, RemoteIP, Port, From+1, To).