#!/bin/bash -e
#########################################################
# Installation of jq application for parsing JSON files #
#########################################################
BLUEPRINT_PATH='/opt/manager/resources/blueprints'
JQ_PATH=$BLUEPRINT_PATH/$(ctx blueprint id)/scripts/jq
if [ ! -f /usr/bin/jq ]; then
    sudo chmod +x $JQ_PATH
    sudo cp $JQ_PATH /usr/bin
fi

dia_id=$(ctx instance runtime_properties external_id)

###################################################################
# Geting token for access to the Openstack API (Cloudify project) #
###################################################################
if [ ! -f /home/centos/token_cloudify.json ]; then
    echo -e "{\n    \"auth\": {\n        \"identity\": {\n            \"methods\": [\n                \"password\"\n            ],\n            \"password\": {\n                \"user\": {\n                    \"domain\": {\n                        \"name\": \"Default\"\n                    },\n                    \"name\": \"cloudify\",\n                    \"password\": \"cloudify\"\n                }\n            }\n        },\n        \"scope\": {\n            \"project\": {\n                \"domain\": {\n                    \"name\": \"Default\"\n                },\n                \"name\": \"cloudify\"\n            }\n        }\n    }\n}" >> /home/centos/token_cloudify.json
fi
TOKEN_CLY=$(curl -si -d @token_cloudify.json -H "Content-type: application/json" http://192.168.203.210:5000/v3/auth/tokens | awk '/X-Subject-Token/ {print $2}')

###################################################################
# Geting Dia instance ip address from cloudify-management-network #
# for instance configuration                                      #
###################################################################
while [[ -z "$INST_IP" ]]
do
    INST_IP=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers/${dia_id} | jq '.server.addresses."'"cloudify-management-network"'"[].addr'  | tr -d '"')
    sleep 1
done
ctx instance runtime_properties ip ${INST_IP}

##########################################################################
# Geting information (ip, mac-address, id) about Dia eth0 and eth1 ports #
# via Openstack API for needed port configuration                        #                                 #
##########################################################################
PAY_PORT_IP=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers/${dia_id} | jq '.server.addresses.payload_network[].addr' | tr -d '"')
PAY_PORT_MAC=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers/${dia_id} | jq '.server.addresses.payload_network[]."OS-EXT-IPS-MAC:mac_addr"' | tr -d '"')
PAY_PORT_ID=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:9696/v2.0/ports | jq '.ports[] | select(.fixed_ips[].ip_address == "'"$PAY_PORT_IP"'")' | jq '.id' | tr -d '"')
CM_PORT_IP=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers/${dia_id} | jq '.server.addresses.control_and_management_network[].addr' | tr -d '"')
CM_PORT_ID=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:9696/v2.0/ports | jq '.ports[] | select(.fixed_ips[].ip_address == "'"$CM_PORT_IP"'")' | jq '.id' | tr -d '"')

ctx instance runtime_properties dia_mac ${PAY_PORT_MAC}

#############################################################
# Get token for access to the Openstack API (Admin project) #
#############################################################
if [ ! -f /home/centos/token.json ]; then
    echo -e "{\n    \"auth\": {\n        \"identity\": {\n            \"methods\": [\n                \"password\"\n            ],\n            \"password\": {\n                \"user\": {\n                    \"domain\": {\n                        \"name\": \"Default\"\n                    },\n                    \"name\": \"admin\",\n                    \"password\": \"openstack\"\n                }\n            }\n        },\n        \"scope\": {\n            \"project\": {\n                \"domain\": {\n                    \"name\": \"Default\"\n                },\n                \"name\": \"admin\"\n            }\n        }\n    }\n}" >> /home/centos/token.json
fi
TOKEN=$(curl -si -d @token.json -H "Content-type: application/json" http://192.168.203.210:5000/v3/auth/tokens | awk '/X-Subject-Token/ {print $2}')

#######################################################
# Preparing configuration JSON file for Dia eth1 port #
#######################################################
if [ -f /home/centos/pay_port.json ]; then
    rm -rf /home/centos/pay_port.json
fi
while [[ -z "$ip_for_pair" ]]
do 
    ctx logger info "ip_for_pair = ${ip_for_pair}"
    ip_for_pair=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:9696/v2.0/ports | jq '.ports[] | select(.name == "vSwitch_eth3_port")' | jq '.fixed_ips[].ip_address' | tr -d '"')
    sleep 1
done
pay_port_name1=$(ctx instance id)
pay_port_name2=_eth1_port
echo -e "{\n    \"port\": {\n    \"name\": \"$pay_port_name1$pay_port_name2\",\n    \"allowed_address_pairs\": [\n        {\n            \"ip_address\": \"${ip_for_pair}\",\n            \"mac_address\": \"${PAY_PORT_MAC}\"\n        }\n    ],\n    \"security_groups\": [\n        \"${security_group}\"\n    ]\n    }\n}" >> /home/centos/pay_port.json

###########################
# Configure Dia eth1 port #
###########################
curl -g -i -X PUT http://192.168.203.210:9696/v2.0/ports/${PAY_PORT_ID} -H "Content-Type: application/json" -H "Accept: application/json" -H "X-Auth-Token: ${TOKEN}" -d @pay_port.json

#######################################################
# Preparing configuration JSON file for Dia eth0 port #
#######################################################
if [ -f /home/centos/cm_port.json ]; then
    rm -rf /home/centos/cm_port.json
fi
cm_port_name1=$(ctx instance id)
cm_port_name2=_eth0_port
echo -e "{\n    \"port\": {\n    \"name\": \"$cm_port_name1$cm_port_name2\",\n    \"security_groups\": [\n        \"${security_group}\"\n    ]\n    }\n}" >> /home/centos/cm_port.json

###########################
# Configure Dia eth0 port #
###########################
curl -g -i -X PUT http://192.168.203.210:9696/v2.0/ports/${CM_PORT_ID} -H "Content-Type: application/json" -H "Accept: application/json" -H "X-Auth-Token: ${TOKEN}" -d @cm_port.json

rm -rf /home/centos/pay_port.json
rm -rf /home/centos/cm_port.json

################################################
## Create propertie for access from blueprint ##
################################################
inst_id=$(ctx instance id)
ctx instance runtime_properties inst_id ${inst_id}