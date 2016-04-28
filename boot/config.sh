#!/bin/bash

usage() {
cat <<HELP
The config CLI command is used to configure the Erlang nodes for distributed Erlang Controller application.
Usage: $0 -c1ip=<the first controller instance Ip>  -c2ip=<the second controller instance Ip>  
-c3ip=<the thrid controller instance Ip> [-h]

Example:
$0 -c1ip='1.2.3.4' -c2ip='2.3.4.5' -c3ip='3.4.5.6'

c1ip parameter specifies the instance where Erlang Controller App will be running;
c2ip parameter specifies the instance where Erlang Controller App will be takeovered if c1ip 
is down;
c3ip parameter specifies the instance where Erlang Controller App will be takeovered if c1ip and c2ip 
are down.

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
    echo "${C3IP}"
    shift # past argument=value
    ;;
    --default)
    DEFAULT=YES
    shift # past argument with no value
    ;;
    *)
     printf "Missing the mandatory arguments!\n"       # unknown option
     usage
     exit 0 
    ;;
esac
done

echo "A first IP: " ${C1IP}
echo "A second IP: " ${C2IP}
echo "A thrid IP: " ${C3IP}

if [ "$C1IP" != "" ] && [ "$C2IP" != "" ] && [ "$C3IP" != "" ]
then
	grep 'controllerA' -P -R -I -l  controller/config/* | xargs sed -i 's/\(controllerA@[.0-9]*\)/controllerA@'`echo "$C1IP"`'/g'
	grep 'controllerB' -P -R -I -l  controller/config/* | xargs sed -i  's/\(controllerB@[.0-9]*\)/controllerB@'`echo "$C2IP"`'/g'
	grep 'controllerC' -P -R -I -l  controller/config/* | xargs sed -i  's/\(controllerC@[.0-9]*\)/controllerC@'`echo "$C3IP"`'/g'

else
	echo "Please specify three IP addresses of the instances where the Erlang Controller application should be distributed!"
	usage
	exit 0
fi
