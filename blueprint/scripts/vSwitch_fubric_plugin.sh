#!/bin/bash
# $1 - controller1 eth0 ip ( 10.10.0.55 )
# $2 - controller2 eth0 ip ( 10.10.0.56 )
# $3 - controller3 eth0 ip ( 10.10.0.57 )
# $4 - vStwitch eth3 ip ( 10.10.2.2 )
# $5 - vStwitch eth1 ip ( 10.10.1.56 )

sudo sh -c 'echo "127.0.1.1 `hostname`" >> /etc/hosts'
sudo sh -c 'echo "10.67.79.3 cloudify-manager-server" >> /etc/hosts'
sudo touch /etc/network/interfaces.d/eth1.cfg
sudo sh -c 'echo "auto eth1\niface eth1 inet dhcp" > /etc/network/interfaces.d/eth1.cfg'
sudo ifup eth1
sudo touch /etc/network/interfaces.d/eth2.cfg
sudo sh -c 'echo "auto eth2\niface eth2 inet dhcp" > /etc/network/interfaces.d/eth2.cfg'
sudo ifup eth2
sudo touch /etc/network/interfaces.d/eth3.cfg
sudo sh -c 'echo "auto eth3\niface eth3 inet dhcp" > /etc/network/interfaces.d/eth3.cfg'
sudo ifup eth3
ODL_IP="tcp:$1 tcp:$2 tcp:$3"
EXT_INTERFACE=$(/sbin/ifconfig |grep -B1 "inet addr:$4" | sed -n '1 p' | awk '{print($1)}')
INT_INTERFACE=$(/sbin/ifconfig |grep -B1 "inet addr:$5" | sed -n '1 p' | awk '{print($1)}')
EXT_IP=$4
INT_IP=$5
sudo ovs-vsctl add-br ovsbr0
sudo ovs-vsctl set bridge ovsbr0 protocols=OpenFlow13
sudo ovs-vsctl set bridge ovsbr0 other_config:datapath-id=0000000000000001
sudo ovs-vsctl set-controller ovsbr0 $ODL_IP
sudo ovs-vsctl add-port ovsbr0 $EXT_INTERFACE
sudo ovs-vsctl add-port ovsbr0 $INT_INTERFACE
sudo ip addr add $EXT_IP/24 dev ovsbr0
sudo ip addr add $INT_IP/24 dev ovsbr0
sudo ip link set ovsbr0 address `cat /sys/class/net/$EXT_INTERFACE/address`
sudo ifconfig $EXT_INTERFACE 0
sudo ifconfig $INT_INTERFACE 0
touch /home/nfvpilot/set_del_ctl.sh
cat > /home/nfvpilot/set_del_ctl.sh << EOT
#!/bin/sh
while true; do
    sleep 10
    sudo ovs-vsctl del-controller ovsbr0; sudo ovs-vsctl set-controller ovsbr0 tcp:10.10.0.55 tcp:10.10.0.56 tcp:10.10.0.57 
done
EOT
chmod +x /home/nfvpilot/set_del_ctl.sh
nohup /home/nfvpilot/set_del_ctl.sh &