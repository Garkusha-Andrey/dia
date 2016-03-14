%% @author Aleksandra

%% @doc Record for basic configuration of diameter instances
-record(diaConfig, {diaInstanceId=undefined,
					node = node(),
					peerId=undefined,
                    remotePeerIp=undefined
                    }).
					
%% @doc Record for local IP addresses of diameter instances (how local instances seek for each other)
-record(diaLocalIpAddress, {diaInstanceId=undefined, ipAddress = undefined, macIpAddress = undeifned}).

%% @doc Record for a single local IP address of all diameter instances (how remote peer sees this)
-record(diaIpAddress, {diaInstanceId=undefined, ipAddress = undefined, macIpAddress = undefined}).

-record(globalData, {ovsIp = {undefined, undefined},
					 ovsMacIp = undefined,
					 publicIp = {undefined,undefined},
					 extGMacIp = undefined}).

-record(instanceWeight, {diaInstanceId=undefined,
                         weight=undefined}).