%% @author Aleksandra

%% @doc Record for basic configuration of diameter instances
-record(diaConnections, {diaInstanceId=undefined,
			node = node(),
			peerId=undefined,
			remotePeerIp=undefined
                    }).
					
%% @doc Record for local IP addresses of diameter instances (how local instances seek for each other)
-record(diaLocalConfig, {diaInstanceId=undefined, ipAddress = undefined, macIpAddress = undeifned}).

%% @doc Record for the pre-defined parameters
-record(globalData, {ovsIpMask     = {undefined, undefined}, %% {"Backplane IP", "Backplane IP Mask"}
		     publicIpMask  = {undefined,undefined},  %% {"Diameter Service IP, Diameter Service Mask"}
		     ovsMac   = undefined,
		     extGwMac = undefined}).

-record(instanceWeight, {diaInstanceId=undefined,
                         weight=undefined}).

-record(servers, {port = undefined,
				  ipaddress = undefined,
				  realmId = undefined,
				  realmHost = undefined,				  				  
				  nodeId = undefined,
				  processId = undefined}).
-record(diameterNodes, {node =  undefined}).
