-module(client_deploy).

-export([start/2,
		 start_traffic/1,
		 add_clients/4]).

-define(RIP, remote_ip).
%%-define(LOCAL_IP, '127.0.0.1').
%%-define(REMOTE_IP, '127.0.0.1').
-define(SERVER_PORT, '3911').
-define(TRAFFIC_INTERVAL, 500).

start(LocalIP, RemoteIp) ->
	io:format("Start of Erlang client_deploy~n"),
	add_clients('c', LocalIP, RemoteIp, 5).

add_clients(Pattern, LocalIP, RemoteIp, Count) ->
	io:format("Adding ~p clients: ~n", [Count]),
	add_clients_pattern(Pattern, 'ex.com', LocalIP, RemoteIp, ?SERVER_PORT, 0, Count).

start_traffic(Count) ->
	continue_call_stub(Count).

continue_call_stub(0) ->
	ok;
continue_call_stub(Count) ->
	continues_call(c_1, "s1", "ex1.com", ?TRAFFIC_INTERVAL),
	continues_call(c_2, "s2", "ex2.com", ?TRAFFIC_INTERVAL),
	continues_call(c_3, "s3", "ex3.com", ?TRAFFIC_INTERVAL),
	continues_call(c_4, "s4", "ex4.com", ?TRAFFIC_INTERVAL),
	continue_call_stub(Count-1).

continues_call(Name, Realm, Host, IntervalMS) ->
	io:format("lient:call(~p ~p ~p ~p ~n", [Name, user, Realm, Host]),
	client:call(Name, user, Realm, Host),
	timer:sleep(IntervalMS)
	.

%% 'c1','ex.ru','127.0.0.1','127.0.0.1','3911'
add_clients_pattern(NamePattern, Realm, LocalIP, RemoteIP, Port, To, To) ->
	ok;

add_clients_pattern(NamePattern, Realm, LocalIP, RemoteIP, Port, From, To) ->
	ServiceName = list_to_atom(lists:concat([NamePattern, "_", From])),
	client:deploy([ServiceName, Realm, LocalIP, RemoteIP, Port]),
	add_clients_pattern(NamePattern, Realm, LocalIP, RemoteIP, Port, From+1, To).