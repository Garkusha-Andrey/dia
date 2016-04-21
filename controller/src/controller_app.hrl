%% @author Aleksandra

%% @doc Record for basic configuration of diameter instances
-record(diaConnections, {nodeId = node(),
					peerId=undefined,
                    remotePeerIp=undefined
                    }).
					
%% @doc Record for local IP addresses of diameter instances (how local instances seek for each other)
-record(diaLocalConfig, {nodeId=undefined, ipAddress = undefined, macIpAddress = undeifned}).

%% @doc Record for the pre-defined parameters
-record(globalData, {ovsIpMask     = {undefined, undefined}, %% {"Backplane IP", "Backplane IP Mask"}
		     publicIpMask  = {undefined,undefined},  %% {"Diameter Service IP, Diameter Service Mask"}
		     ovsMac   = undefined,
		     extGwMac = undefined}).


-record(instanceWeight, {dianodeId=undefined,
                         weight=undefined}).

-record(servers, {portIpAddr = {undefined,undefined},
				  realmId = undefined,
				  realmHost = undefined,				  				  
				  nodeId = undefined,
				  processId = undefined}).
-record(clients, {portIpAddr = {undefined, undefined}, nodeId = udefined}).
-record(diameterNodes, {node =  undefined}).