%% @author Aleksandra
%% @doc @todo Add description to controller_sub.

%% @doc Record for basic configuration of diameter instances
-record(diaConfig, { peerId=undefined,
                    remotePeerIp=undefined,
                    diaInstanceId=undefined}).
					
%% @doc Record for local IP addresses of diameter instances (how local instances seek for each other)
-record(diaLocalIpAddress, {node = node(), ipAddress = undefined}).

%% @doc Record for a single local IP address of all diameter instances (how remote peer sees this)
-record(diaIpAddress, {node = node(), ipAddress = undefined}).