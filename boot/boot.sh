#!/bin/bash

usage() {
cat <<HELP
The boot CLI command is used to start up the Enode in vHost.
Usage: $0 [-a=<application>] [-rnode=<remote node>] [-inst=<diaInstId>] \
[-netIf=<Interface name>] \
[-ovsIntIp=<OVSIntIp>] [-ovsIntMask=<OVSIntMask>] \
[-publicIp=<Diameter Public IP>] [-publicMask=<Diameter Public Mask>] \
[-ovsMac]=<OVS MAC>] [-extGwMac=<External Gateway MAC>] \
[-contrprio=<Priorited of the controller app>][-h]

If you are going to start Controller Erlang application, you need specify the following parameters:
[-rnode=<remote node>] [-netIf=<Interface name>] \
[-ovsIntIp=<OVSIntIp>] [-ovsIntMask=<OVSIntMask>] \
[-publicIp=<Diameter Public IP>] [-publicMask=<Diameter Public Mask>] [-ovsMac]=<OVS MAC>] \
[-extGwIp=<External Gateway MAC>] [-contrprio=<Priorited of the controller app>]

-rnode parameter is not needed for starting of the first Controller Enode, but it is mandatory for the starting the next Controller Enodes.
-contrprio parameter specifies the priority of the distributed Controller App, it could be equal to A,B or C. Controller App with prio A will 
be running firstly; B has the second priority, Controller App will be running on B inst if A is down; C has the thrid priority, 
Controller App will be running on C inst if A an B are down.

Routing parameters:
-netIf                - local network interface to be used for erlang communication
-ovsIntIp -ovsIntMask - IP address+mask of the OVS instance in the internal payload network
-publicIp -publicMask - public IP address+mask of the Diameter service
-ovsMac               - MAC address of the OVS instance
-extGwMac             - MAC address of the external gateway

If you are going to start some other Erlang application, just specify the application name, using the following parameter:
-a=<application>

HELP
    exit 0
}
for i in "$@"
do
#echo $i
case $i in
    -h|--help)
    usage
       exit
    ;;	
    -a=*|--application=*)
    APPLICATION="${i#*=}"
    shift # past argument=value
    ;;
    -rnode=*)
    RNODE="${i#*=}"
    shift # past argument=value
    ;;
    -inst=*|--diaInstId=*)
    DIAINSTID="${i#*=}"
    shift # past argument=value
    ;;
    -diaMac=*)
    DIAMETER_MAC="${i#*=}"
    shift # past argument=value
    ;;
    -netIf=*)
    NetIf="${i#*=}"
    shift # past argument=value
    ;;
    -ovsIntIp=*)
    OVSIntIp="${i#*=}"
    shift # past argument=value
    ;;
    -ovsIntMask=*)
    OVSIntMask="${i#*=}"
    shift # past argument=value
    ;;
    -publicIp=*)
    PublicIp="${i#*=}"
    shift # past argument=value
    ;;
    -publicMask=*)
    PublicMask="${i#*=}"
    shift # past argument=value
    ;;
    -ovsMac=*)
    OVSMac="${i#*=}"
    shift # past argument=value
    ;;
    -extGwMac=*)
    ExtGwMac="${i#*=}"
    shift # past argument=value
    ;;
    -contrprio=*)
    CPRIO="${i#*=}"
    shift # past argument=value
    ;;
    --default)
    DEFAULT=YES
    shift # past argument with no value
    ;;
    *)
    printf "Unknown option!\n"       # unknown option
    usage
    exit 0 
    ;;
esac
done

LOCALIP=`ifconfig $NetIf | grep 'inet addr:' | cut -d: -f2 | awk '{print $1 }'`
echo "---------------------------------------------"
echo "Application:" $APPLICATION
echo "dialocalIp:" $LOCALIP
echo "diaInstId:" $DIAINSTID
echo "default:" $DEFAULT

echo "OVS internal IP/mask:" $OVSIntIp"/"$OVSIntMask
echo "Public IP/mask:" $PublicIp"/"$PublicMask
echo "OVS MAC:" $OVSMac
echo "External gateway MAC:" $ExtGwMac

echo "the config will be used:" $CPRIO
echo "---------------------------------------------"; echo

if [ $APPLICATION = "" ]; then
	echo "Empty Appl"
	if [ "$OVSIntIp" != "" ] && [ "$OVSIntMask" != "" ] && [ "$PublicIp" != "" ] && [ "$PublicMask" != "" ] && [ "$ExtGwMac" != "" ] && [ "$OVSMac" != "" ]
	then
		if [ "$CPRIO" != "" ]
		then
			Enode=controller`echo $CPRIO`@`echo $LOCALIP`
			setsid erl -detached -name $Enode -s boot start -s controller_app change_configuration \
			    $OVSIntIp $OVSIntMask $PublicIp $PublicMask $OVSMac $ExtGwMac \
			    -setcookie 'ABCD' \
			    -config ../config/controller`echo $CPRIO`.config >/dev/null 2>&1 < /dev/null
		else
			echo "You are trying to start the controller application. You need specify the configuration parameters of Diameters!\n"
			usage
			exit 0
		fi
	fi
else
	if [ "$APPLICATION" = "controller_app" ]; then
		echo "Application controller_app!"
		if [ "$CPRIO" != "" ]
		then
			if [ "$OVSIntIp" != "" ] && [ "$OVSIntMask" != "" ] && \
				[ "$PublicIp" != "" ] && [ "$PublicMask" != "" ] && \
				[ "$ExtGwMac" != "" ] && [ "$OVSMac" != "" ]
			then
				if [ "$RNODE" != "" ]
				then
					echo "Then != controller_app"
					Enode=controller`echo $CPRIO`@`echo $LOCALIP`
					setsid erl -detached -name $Enode -s boot start $RNODE -s controller_app change_configuration \
					    $OVSIntIp $OVSIntMask $PublicIp $PublicMask $OVSMac $ExtGwMac \
					    -setcookie 'ABCD' \
					    -config ../config/controller`echo $CPRIO`.config >/dev/null 2>&1 < /dev/null
				else
					Enode=controller`echo $CPRIO`@`echo $LOCALIP`
					setsid erl -detached -name $Enode -s boot start -s $APPLICATION change_configuration \
					    $OVSIntIp $OVSIntMask $PublicIp $PublicMask $OVSMac $ExtGwMac \
					    -setcookie 'ABCD' \
					    -config ../config/controller`echo $CPRIO`.config >/dev/null 2>&1 < /dev/null
				fi
			else
				echo "You are trying to start the controller application. You need specify the configuration parameters of Diameters!\n"
				usage
				exit 0
			fi
		else
			echo "You are trying to start the controller application. You need specify the priority of Controller App!\n"
			Enode=controller`echo $CPRIO`@`echo $LOCALIP`
					setsid erl -detached -name $Enode -s application start inets -s boot start -s $APPLICATION change_configuration \
					    $OVSIntIp $OVSIntMask $PublicIp $PublicMask $OVSMac $ExtGwMac \
					    -setcookie 'ABCD' >/dev/null 2>&1 < /dev/null 
		fi
	else
		if [ "$APPLICATION" = "diameter" ]; then
			echo "Diameter!"
			if [ "$RNODE" != "" ] && [ $DIAINSTID != "" ] && \
				[ "$PublicIp" != "" ] && [ $DIAMETER_MAC != "" ]
			then
				echo "boot diam with rnode"
				DEnode=diameter_`echo $DIAINSTID`@`echo $LOCALIP`
				InboundRalayName=ir`echo $DIAINSTID`
				REALM='nfv.ru'
				INBOUND_RELAY_PORT=3911
				setsid erl -detached -name $DEnode \
				 -s boot start $RNODE \
				 -s controller_app change_configuration \
				 -s irelay deploy $InboundRalayName $REALM $PublicIp $INBOUND_RELAY_PORT \
				 -s relay_manager start $PublicIp \
				  diameter $DEnode $LOCALIP $DIAMETER_MAC -setcookie 'ABCD' >/dev/null 2>&1 < /dev/null
			else
				echo "You need specify the one Controller node to start Diameter instance!\n"
				usage
				exit 0
			fi
		else
			echo "Undefined Application!\n"
			usage
			exit 0
		fi			
	fi

fi
