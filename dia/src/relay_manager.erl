%% @author agarkush
%% @doc @todo Add description to relay_manager.


-module(relay_manager).

-include_lib("../../controller/src/controller_app.hrl").
-include_lib("dia_relay_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).

-export([ping_add_srv/0,
		 ping_rm_srv/0]).

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

	ets:new(?RELAY_MANAGER_SERVER_TABLE, [set, public, named_table]),

	Index = 0,
	register(?MODULE, self()),
	relay_manager_listener(SwitchIP, Index),
	ok.


%% Just for tests without controller
ping_add_srv() ->
	Srv = #servers{portIpAddr = {1234, "127.0.0.1"},
				   realmId = "s1.ex.com", realmHost="ex.com"},
	{relay_manager, node()} ! {add_server, Srv}.

ping_rm_srv() ->
	Srv = #servers{portIpAddr = {1234, "127.0.0.1"},
				   realmId = "s1.ex.com", realmHost="ex.com"},
	{relay_manager, node()} ! {rm_server, Srv}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Waiting for msgs from controller to add server to this node.
relay_manager_listener(IPsrc, Index) ->
	receive
		{add_server, Server}
			when is_record(Server, servers) ->

			RealmID = ?REALM_ID,
			ServiceName = list_to_atom(lists:concat(["server_",Index])),
			IPAddr = Server#servers.portIpAddr,
			{Portdst, IPdst_name} = IPAddr,
			{ok, IPdst} = inet_parse:address(IPdst_name),
			
			Result = ets:lookup(?RELAY_MANAGER_SERVER_TABLE, IPAddr),
			case Result of
				[] ->
					spawn(orelay, deploy, [[ServiceName, RealmID, IPsrc, IPdst, Portdst]]),
					ets:insert(?RELAY_MANAGER_SERVER_TABLE, {IPAddr, ServiceName}),
					io:format("relay_manager: Add server: ~p ~p ~p ~p ~p ~n", [ServiceName, RealmID, IPsrc, IPdst, Portdst]);
				Something ->
					io:format("relay_manager: Add server: Has something ~p ~n", [Something])
			end,

			io:format("relay_manager: Add server: End of edding ~n");
		{rm_server, Server}
			when is_record(Server, servers) ->
			io:format("relay_manager: Remove server: Just get server to remove ~p~n", [Server]),
			
			IPAddr = Server#servers.portIpAddr,
			io:format("relay_manager: Remove server: IPAddr  ~p ~n", [IPAddr]),
 			Result = ets:lookup(?RELAY_MANAGER_SERVER_TABLE, IPAddr),
			io:format("relay_manager: Remove server: Result  ~p ~n", [Result]),
			case Result of
				[{_,ServiceName}] ->
					io:format("relay_manager: Remove server: ServiceName  ~p ~n", [ServiceName]),
					ets:delete(?RELAY_MANAGER_SERVER_TABLE, IPAddr),
					orelay:stop(ServiceName);
				[] ->
					do_nothing;
				Another ->
					io:format("relay_manager: Remove server: Strange result ~p ~n", [Another])
			end,
			io:format("relay_manager: Remove server: End~n");
		UnexpectedMsg ->
			io:format("relay_manager: received an unexpected msg: ~w ~n", [UnexpectedMsg])
	end,

	relay_manager_listener(IPsrc, Index+1).
