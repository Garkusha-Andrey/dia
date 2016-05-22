-module(client_deploy).

-export([start/0,
		 start_traffic/0]).

start() ->
	io:format("Start of Erlang client_deploy~n").

start_traffic() ->
	continue_call_stub().

continue_call_stub() ->
	continues_call(c1, "s1", "ex1.com", 2000),
	continues_call(c1, "s2", "ex2.com", 2001),
	continues_call(c1, "s3", "ex3.com", 2002),
	continues_call(c1, "s4", "ex4.com", 2003),
	continues_call(c1, "s5", "ex5.com", 2004).

continues_call(Name, Realm, Host, IntervalMS) ->
	client:call(Name, user, Realm, Host),
	timer:sleep(IntervalMS)
	.
