#!/bin/bash
# $1 - vSwitch_eth1_port fixed_ip_address (ovsIntIp)
# $2 - vSwitch_eth3_port port_mac (ovsMac)
# $3 - Router gateway mac-address (extGwMac)
# $4 - controller priority parameter (contrprio)
# $5 - vSwitch_eth3_port fixed_ip_address (publicIp)
# $6 - controller1_eth0_port fixed_ip_address
# $7 - controller2_eth0_port fixed_ip_address
# $8 - controller3_eth0_port fixed_ip_address

# Copy repository from cloudify manager instance
sudo sshpass -p 'apprepo' scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -r apprepo@10.67.79.3:/home/apprepo/diameter-relay-agent /home/nfvpilot
# Preperin parameters for script
MASK=255.255.255.0
INTERFACE=$(/sbin/ifconfig |grep -B1 "inet addr:10.10.0.5[5-7]" | sed -n '1 p' | awk '{print($1)}')
cd /home/nfvpilot/diameter-relay-agent
boot/config.sh -c1ip=$6 -c2ip=$7 -c3ip=$8
cd /home/nfvpilot/diameter-relay-agent/controller/ebin
sleep 180
../../boot/boot.sh -a=controller_app -netIf=$INTERFACE -ovsIntIp=$1 -ovsIntMask=$MASK -publicIp=$5 -publicMask=$MASK -ovsMac=$2 -extGwMac=$3 -contrprio=$4 >> /home/nfvpilot/script_result