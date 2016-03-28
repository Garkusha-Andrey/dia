#!/bin/bash

usage() {
cat <<HELP
The boot CLI command is used to start up the Enode in vHost.
Usage: $0 [-a=<application>] [-rnode=<remote node>] [-inst=<diaInstId>] [-ovsIpInt=<OVSIpInt>] [-ovsMInt=<OVSMaskIpInt>] \
[-ovspubip=<OVS Public IP>] [-ovspubm=<OVS Public Mask>] [-ovsmac]=<OVS MAC IP>] [-extgip=<External Gateway Ip>] \
[-contrprio=<Priorited of the controller app>][-h]

If you are going to start Controller Erlang application, you need specify the following parameters:
[-rnode=<remote node>] [-ovsIpInt=<OVSIpInt>] [-ovsMInt=<OVSMaskIpInt>] \
[-ovspubip=<OVS Public IP>] [-ovspubm=<OVS Public Mask IP>] [-ovsmac]=<OVS MAC IP>] \
[-extgip=<External Gateway Ip>] [-contrprio=<Priorited of the controller app>]

-rnode parameter is not needed for starting of the first Controller Enode, but it is mandatory for the starting the next Controller Enodes.
-contrprio parameter specifies the priority of the distributed Controller App, it could be equal to A,B or C. Controller App with prio A will 
be running firstly; B has the second priority, Controller App will be running on B inst if A is down; C has the thrid priority, 
Controller App will be running on C inst if A an B are down.

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
    echo "${APPLICATION}"
    shift # past argument=value
    ;;
    -rnode=*)
    RNODE="${i#*=}"
    echo "${RNODE}"
    shift # past argument=value
    ;;
    -inst=*|--diaInstId=*)
    DIAINSTID="${i#*=}"
    shift # past argument=value
    ;;
    -ovsIpInt=*)
    OVSIpInt="${i#*=}"
    echo "${OVSIpInt}"
    shift # past argument=value
    ;;
    -ovsMInt=*)
    OVSMASKIPINT="${i#*=}"
    echo "${OVSMASKIPINT}"
    shift # past argument=value
    ;;
    -extgip=*)
    ExtGIp="${i#*=}"
    echo "${ExtGIp}"
    shift # past argument=value
    ;;
    -ovspubip=*)
    OVSPubIp="${i#*=}"
    echo "${OVSPubIp}"
    shift # past argument=value
    ;;
    -ovsmac=*)
    OVSMAC="${i#*=}"
    echo "${OVSMAC}"
    shift # past argument=value
    ;;
    -ovspubm=*)
    OVSPubMask="${i#*=}"
    echo "${OVSPubMask}"
    shift # past argument=value
    ;;
    -contrprio=*)
    CPRIO="${i#*=}"
    echo "${CPRIO}"
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

LOCALIP=`ifconfig wlan0 | grep 'inet addr:' | cut -d: -f2 | awk '{print $1 }'`
MACLOCALIP=`ifconfig wlan0 | grep 'HWaddr' | awk '{print $5 }'`
echo "Application: " ${APPLICATION}
echo "Enode: " $ENODE
echo "PeerId: " $PEERID
echo "dialocalIp: " $LOCALIP
echo "DiaIp: " $DIAIP
echo "RemotePeerId: " $RPEERID
echo "diaInstId: " $DIAINSTID
echo "macLocalIp:" $MACLOCALIP
echo "default: " $DEFAULT
echo "OVS IP: " $OVSIp
echo "OVS mac: " $OVSMACIP
echo "the config will be used:" $CPRIO

echo "\nEnodeId: " $ENODEN
 

if [ $APPLICATION="" ]; then
	echo "Empty Appl"
	if [ "$OVSIpInt" != "" ] && [ "$OVSMASKIPINT" != "" ] && [ "$OVSPubMask" != "" ] && [ "$ExtGIp" != "" ] && [ "$OVSPubIp" != "" ] && [ "$OVSMAC" != "" ]
	then
		if [ "$CPRIO" != "" ]
		then
			Enode=controller`echo $CPRIO`@`echo $LOCALIP`
			erl -name $Enode -s boot start -s controller_app change_configuration $OVSIpInt $OVSMASKIPINT \
			$ExtGIp $OVSPubIp $OVSMAC $OVSPubMask -setcookie 'ABCD' \
			-config ../config/controller`echo $CPRIO`.config
		else
			echo "You are trying to start the controller application. You need specify the configuration parameters of Diameters!\n"
			usage
			exit 0
		fi
	else
		if [ "$RNODE" != "" ] && [ $DIAINSTID != "" ]
		then
			echo "boot diam with rnode"
			DEnode=diameter`echo $DIAINSTID`@`echo $LOCALIP`
			erl -name $DEnode -s boot start $RNODE -s controller_app change_configuration \
			diameter $DEnode $LOCALIP $DIAINSTID $MACLOCALIP -setcookie 'ABCD'
		else
			echo "You need specify the one Controller node to start Diameter instance!\n"
			usage
			exit 0
		fi
	fi
else
	if [ "$APPLICATION" = "controller_app" ]; then
		echo "Application controller_app!"
		if [ "$CPRIO" != "" ]
		then
			if [ "$OVSIpInt" != "" ] && [ "$OVSMASKIPINT" != "" ] \
			&& [ "$ExtGIp" != "" ] && [ "$OVSPubIp" != "" ] && [ "$OVSMAC" != "" ] && [ "$OVSPubMask" != "" ]
			then
				if [ "$RNODE" != "" ]
				then
					echo "Then != controller_app"
					Enode=controller`echo $CPRIO`@`echo $LOCALIP`
					erl -name $Enode -s boot start $RNODE -s controller_app change_configuration \
					$OVSIpInt $OVSMASKIPINT $ExtGIp $OVSPubIp $OVSMAC $OVSPubMask -setcookie 'ABCD' \
					-config ../config/controller`echo $CPRIO`.config
				else
					Enode=controller`echo $CPRIO`@`echo $LOCALIP`
					erl -name $Enode -s boot start -s $APPLICATION change_configuration \
					$OVSIpInt $OVSMASKIPINT $ExtGIp $OVSPubIp $OVSMAC \
					$OVSPubMask -setcookie 'ABCD' -config ../config/controller`echo $CPRIO`.config
				fi
			else
				echo "You are trying to start the controller application. You need specify the configuration parameters of Diameters!\n"
				usage
				exit 0
			fi
			echo "You are trying to start the controller application. You need specify the priority of Controller App!\n"
			usage
			exit 0
		fi
	else
		if [ "$APPLICATION" = "diameter" ]; then
			echo "Diameter!"
			if [ "$RNODE" != "" ] && [ $DIAINSTID != "" ]
			then
				echo "boot diam with rnode"
				DEnode=diameter`echo $DIAINSTID`@`echo $LOCALIP`
				erl -name $DEnode -s boot start $RNODE -s controller_app change_configuration \
				diameter $DEnode $LOCALIP $DIAINSTID $MACLOCALIP -setcookie 'ABCD'
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