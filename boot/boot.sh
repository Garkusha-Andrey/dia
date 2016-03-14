#!/bin/bash

usage() {
cat <<HELP
The boot CLI command is used to start up the Enode in vHost.
Usage: $0 [-a=<application>] [-rnode=<remote node>] [-p=<peerId>] [-dip=<diaIp>] \
[-rp=<remotePeerId>] [-inst=<diaInstId>] [-dmip=<diaMacIp>] [-h]

If you are going to start Controller Erlang application, you need specify the following parameters:
[-rnode=<remote node>] -p=<peerId> -dip=<diaIp> \
-rp=<remotePeerId> -inst=<diaInstId> -dmip=<diaMacIp>
-rnode parameter is optional for starting of the first Controller Enode, but it is mandatory for the starting the next Controller Enodes.

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
    -p=*|--peerId=*)
    PEERID="${i#*=}"
    shift # past argument=value
    ;;
    -dip=*|--diaIp=*)
    DIAIP="${i#*=}"
    shift # past argument=value
    ;;
    -rp=*|--rpeerId=*)
    RPEERID="${i#*=}"
    shift # past argument=value
    ;;
    -inst=*|--diaInstId=*)
    DIAINSTID="${i#*=}"
    shift # past argument=value
    ;;
    -dmip=*|--diaMacIp=*)
    DIAMACIP="${i#*=}"
    shift # past argument=value
    ;;
    --default)
    DEFAULT=YES
    shift # past argument with no value
    ;;
    -c1ip=*)
    C1IP="${i#*=}"
    echo "${C1IP}"
    shift # past argument=value
    ;;
    -c2ip=*)
    C2IP="${i#*=}"
    echo "${C2IP}"
    shift # past argument=value
    ;;
    -c3ip=*)
    C3IP="${i#*=}"
    echo "${C2IP}"
    shift # past argument=value
    ;;
    *)
     printf "Missing the mandatory arguments!\n"       # unknown option
     usage
     exit 0 
    ;;
esac
done


String=`sed '3!d' ../config/a.config`
echo $String

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

ENODEN=controler`echo $DIAINSTID`@`echo $LOCALIP`
echo "\nEnodeId: " $ENODEN
 


if [ $APPLICATION="" ]; then
	echo "Empty Apl"
	if [ "$MACLOCALIP" != "" ] && [ "$PEERID" != "" ] && \
	[ "$LOCALIP" != "" ] && [ "$RPEERID" != "" ] && [ "$DIAINSTID" != "" ] \
	&& [ "$DIAMACIP" != "" ]
	then
		if [ "$RENODE" != "" ]
		then
			erl -name $ENODEN -s boot start $RENODE -s controller_app change_configuration $ENODEN $PEERID $LOCALIP 
		$DIAIP $RPEERID $DIAINSTID $MACLOCALIP $DIAMACIP
		else
			erl -name $ENODEN -s boot start -s controller_app change_configuration $ENODEN $PEERID $LOCALIP 
		$DIAIP $RPEERID $DIAINSTID $MACLOCALIP $DIAMACIP
		fi
	else
		echo "Specify an application that should be started!" 
		usage
		exit 0
	fi
else
	if [ "$APPLICATION" = "controller_app" ]; then
		echo "Application controller_app!"
		if [ "$MACLOCALIP" != "" ] && [ "$PEERID" != "" ] && \
		[ "$LOCALIP" != "" ] && [ "$RPEERID" != "" ] && [ "$DIAINSTID" != "" ] \
		&& [ "$DIAMACIP" != "" ]  
		then
			if [ "$RENODE" != "" ]
			then
			echo "Then != controller_app"
			erl -name $ENODEN  -s boot start $RENODE 
			-s $APPLICATION change_configuration $ENODEN $PEERID $LOCALIP \
			$DIAIP $RPEERID $DIAINSTID $MACLOCALIP $DIAMACIP
		else
			erl -name $ENODEN -s boot start $RENODE 
			-s $APPLICATION change_configuration $ENODEN $PEERID $LOCALIP \
			$DIAIP $RPEERID $DIAINSTID $MACLOCALIP $DIAMACIP
		fi
	else
			echo "You are trying to start the controller application. You need specify the configuration parameters of Diameters!\n"
			usage
			exit 0
		fi
	fi
fi
