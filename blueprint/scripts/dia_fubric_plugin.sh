#!/bin/bash
# $3 - Unique Dia instance number (inst)
# $4 - Diameter port mac address from control and managemetn network (diamac)
sudo sh -c 'echo "127.0.1.1 `hostname`" >> /etc/hosts'
sudo sh -c 'echo "10.67.79.3 cloudify-manager-server" >> /etc/hosts'
sudo touch /etc/network/interfaces.d/eth1.cfg
sudo sh -c 'echo "auto eth1\niface eth1 inet dhcp" > /etc/network/interfaces.d/eth1.cfg'
sudo ifup eth1
sudo touch /etc/network/interfaces.d/eth2.cfg
sudo sh -c 'echo "auto eth2\niface eth2 inet dhcp" > /etc/network/interfaces.d/eth2.cfg'
sudo ifup eth2
SWITCH_IP=$1 # vStwitch eth1 ip ( 10.10.1.56 )
EXT_IP=$2 # vStwitch eth3 ip ( 10.10.2.2 )
sudo route del default
sudo route add default gw $SWITCH_IP
sudo ip addr add $EXT_IP/32 dev lo
sudo sshpass -p 'apprepo' scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -r apprepo@10.67.79.3:/home/apprepo/diameter-relay-agent /home/nfvpilot
cd /home/nfvpilot/diameter-relay-agent/tools
sudo ./install.sh /home/nfvpilot/diameter-relay-agent
INTERFACE=$(/sbin/ifconfig |grep -B1 "inet addr:10.10.0" | sed -n '1 p' | awk '{print($1)}')
cd /home/nfvpilot/diameter-relay-agent/Result
sleep 5
sudo ./boot.sh -a=diameter -netIf=$INTERFACE -rnode=controllerA@10.10.0.55 -inst=$3 -diaMac=$4 -publicIp=$2 >> /home/nfvpilot/script_result
touch /home/nfvpilot/make_ping.sh
cat > /home/nfvpilot/make_ping.sh << EOT
#!/bin/sh
while true; do
    sleep 20
    ping -q -c5 10.10.1.56 > /dev/null
done
EOT
chmod +x /home/nfvpilot/make_ping.sh
nohup /home/nfvpilot/make_ping.sh &