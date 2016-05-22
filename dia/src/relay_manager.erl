%% @author agarkush
%% @doc @todo Add description to relay_manager.


-module(relay_manager).

-include_lib("../../controller/src/controller_app.hrl").
-include_lib("dia_relay_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).

-export([ping/0]).

-export([relay_manager_listener/2]).

-define(RELAY_MANAGER_SERVER_TABLE, server_table).
-define(REALM_ID, 'nfv.ru').

start(T) ->
	
	{ok, Log} = file:open(?LOG_FILE, [append]),
	erlang:group_leader(Log, self()),

    io:format("Just test string ~n"),
	
	SwitchIP = case inet_parse:address(atom_to_list(lists:nth(1, T))) of
		{ok, IP}  		-> IP;
		{error, Reason} -> io:format("Bad Addr: ~w. Reason ~w ~n", [lists:nth(1, T), Reason])
	end,
	
	%% It is not working when starts from bash
	%%ets:new(?RELAY_MANAGER_SERVER_TABLE, [set, public, named_table]),
	
	Index = 0,
	register(?MODULE, spawn(?MODULE, relay_manager_listener, [SwitchIP, Index])),
	ok.


%% Just for tests without controller
ping() ->
	Srv = #servers{portIpAddr = {{127,0,0,1}, 1234},
				   realmId = 111, realmHost='ex.sc'},
	{relay_manager, node()} ! {add_server, Srv}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Waiting for msgs from controller to add server to this node.
relay_manager_listener(IPsrc, Index) ->
	ets:new(?RELAY_MANAGER_SERVER_TABLE, [set, public, named_table, {keypos,4}]),
	receive
		{add_server, Server}
			when is_record(Server, servers) ->

			RealmID = ?REALM_ID,
			ServiceName = list_to_atom(lists:concat(["server_",Index])),
			{Portdst, IPdst_name} = Server#servers.portIpAddr,
			{ok, IPdst} = inet_parse:address(IPdst_name),

			spawn(orelay, deploy, [[ServiceName, RealmID, IPsrc, IPdst, Portdst]]),
			
			%% It is not working when starts from bash
			ets:insert(?RELAY_MANAGER_SERVER_TABLE, {ServiceName, RealmID, IPsrc, {Portdst, IPdst_name}}),
			io:format("relay_manager: Add server: ~w ~w ~w ~w ~w ~n", [ServiceName, RealmID, IPsrc, IPdst, Portdst]);
		{rm_server, Server}
			when is_record(Server, servers) ->

 			{ServiceName, RealmID, IPsrc, {Portdst, IPdst_name}} = ets:lookup_element(?RELAY_MANAGER_SERVER_TABLE,
																	Server#servers.portIpAddr, 4),
			{ok, IPdst} = inet_parse:address(IPdst_name),
			io:format("relay_manager: Remove server: ~w ~w ~w ~w ~w ~n", [ServiceName, RealmID, IPsrc, IPdst, Portdst]),
			orelay:stop(ServiceName);
		UnexpectedMsg ->
			io:format("relay_manager: received an unexpected msg: ~w ~n", [UnexpectedMsg])
	end,

	relay_manager_listener(IPsrc, Index+1).
