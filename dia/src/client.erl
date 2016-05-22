%% How to start client:
%%   client:start(c1, 'ex.ru').
%%   client:connect(c1, {tcp, loopback, loopback, 3911}).
%%   client:call(c1, rar).
%%
%% Or more simple way:
%%	client:deploy([c1, 'ex.ru', {127,0,0,1}, {127,0,0,1}, '3912']).
%%

-module(client).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-export([deploy/1,
		 start/2,     %% start a service
         start/3,     %%
         connect/2,   %% add a connecting transport
         call/2,      %% send using the record encoding
         call/4,      
         cast/1,      %% send using the list encoding and detached
         stop/1]).    %% stop a service
		 
-define(DEF_SVC_NAME, ?MODULE).
-define(CALLBACK_MOD, client_cb).
-define(L, atom_to_list).

%% The service configuration. As in the server example, a client
%% supporting multiple Diameter applications may or may not want to
%% configure a common callback module on all applications.
-define(SERVICE(HostName, Realm),
						[{'Origin-Host', ?L(HostName) ++ "." ++ ?L(Realm)},
                        {'Origin-Realm', ?L(Realm)},
                        {'Vendor-Id', 0},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [0]},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, ?CALLBACK_MOD}]}]).

%% deploy([<Name>, <Ralm>, <local IP>, <remote IP>, <Port>])
%% client:deploy(['c1','ex.ru','127.0.0.1','127.0.0.1','3911']). - from Erlang mashine
%% erl -s client deploy 'c1' 'ex.com' "127.0.0.1" "127.0.0.1" 3911 - from bash
deploy(T) ->
	Name = lists:nth(1, T),
	Realm = lists:nth(2, T),	
	{ok, LIp} = inet_parse:address(atom_to_list(lists:nth(3, T))),
	{ok, RIp} = inet_parse:address(atom_to_list(lists:nth(4, T))),
	Port  = list_to_integer(atom_to_list(lists:nth(5, T))),

    diameter:start(),
	start(Name, Realm),
    connect(Name, {tcp, LIp, RIp, Port}).

%% start/2
start(Name, Realm)
  when is_atom(Name) ->
    start(Name, Realm, []).


%% start/3
start(Name, Realm, Opts) ->
    node:start(Name, Opts ++ [T || {K,_} = T <- ?SERVICE(Name, Realm),
                                   false == lists:keymember(K, 1, Opts)]).

%% connect/2
%% example: connect(client1, 3901)
connect(Name, Port)
  when is_integer(Port) ->
    node:connect(Name, {tcp, loopback, loopback, Port});

connect(Name, T) ->
    node:connect(Name, T).

%% call/1
%% Send base RAR message to server
call(Name, rar) ->
    SId = diameter:session_id(?L(Name)),
    RAR = #diameter_base_RAR{'Session-Id' = SId,
                             'Auth-Application-Id' = 0,
                             'Re-Auth-Request-Type' = 0},
	
	io:format("client.erl::call(~w)~n"
              "Msg: ~p ~n", [Name, RAR]),
    diameter:call(Name, common, RAR, []).

%% call/4
%% Send RAR message to  specific server
%% call(ca, user, "ex1.com", "s1.ex1.com") 
call(Name, user, Realm, Host) ->
    SId = diameter:session_id(?L(Name)),
    RAR = #diameter_base_RAR{'Session-Id' = SId,
                             'Auth-Application-Id' = 0,
                             'Re-Auth-Request-Type' = 0,

                             %% define final destenation [rfc 6733 ch. 6.1]
                             'Destination-Realm' = Realm,
		                     'Destination-Host'  = Host},

	RAR_packet = #diameter_packet{msg = RAR},

    diameter:call(Name, common, RAR_packet, []),
	ok.


%% cast/1
cast(Name) ->
    SId = diameter:session_id(?L(Name)),
    RAR = ['RAR', {'Session-Id', SId},
                  {'Auth-Application-Id', 0},
                  {'Re-Auth-Request-Type', 1}],
	io:format("client.erl::cast(Name)~n SId: ~s\n", [SId]),
    diameter:call(Name, common, RAR, [detach]).

%% stop/1

stop(Name) ->
    node:stop(Name).
