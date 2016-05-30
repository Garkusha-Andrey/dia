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

controller_id=$(ctx instance runtime_properties external_id)

################################################################
# Geting token for access to the Openstack API (Admin project) #
################################################################
if [ ! -f /home/centos/token.json ]; then
    echo -e "{\n    \"auth\": {\n        \"identity\": {\n            \"methods\": [\n                \"password\"\n            ],\n            \"password\": {\n                \"user\": {\n                    \"domain\": {\n                        \"name\": \"Default\"\n                    },\n                    \"name\": \"admin\",\n                    \"password\": \"openstack\"\n                }\n            }\n        },\n        \"scope\": {\n            \"project\": {\n                \"domain\": {\n                    \"name\": \"Default\"\n                },\n                \"name\": \"admin\"\n            }\n        }\n    }\n}" >> /home/centos/token.json
fi
TOKEN=$(curl -si -d @token.json -H "Content-type: application/json" http://192.168.203.210:5000/v3/auth/tokens | awk '/X-Subject-Token/ {print $2}')

###################################################################
# Geting token for access to the Openstack API (Cloudify project) #
###################################################################
if [ ! -f /home/centos/token_cloudify.json ]; then
    echo -e "{\n    \"auth\": {\n        \"identity\": {\n            \"methods\": [\n                \"password\"\n            ],\n            \"password\": {\n                \"user\": {\n                    \"domain\": {\n                        \"name\": \"Default\"\n                    },\n                    \"name\": \"cloudify\",\n                    \"password\": \"cloudify\"\n                }\n            }\n        },\n        \"scope\": {\n            \"project\": {\n                \"domain\": {\n                    \"name\": \"Default\"\n                },\n                \"name\": \"cloudify\"\n            }\n        }\n    }\n}" >> /home/centos/token_cloudify.json
fi
TOKEN_CLY=$(curl -si -d @token_cloudify.json -H "Content-type: application/json" http://192.168.203.210:5000/v3/auth/tokens | awk '/X-Subject-Token/ {print $2}')

##########################################################
# Geting mac-address of vSwitch router via Openstack API #
##########################################################
GW_IP=$(curl -s -H "X-Auth-Token: ${TOKEN}" http://192.168.203.210:9696/v2.0/subnets/${vSwitch_network_subnet_id}  | jq '.subnet.gateway_ip' | tr -d '"')
GW_MAC=$(curl -s -H "X-Auth-Token: ${TOKEN}" http://192.168.203.210:9696/v2.0/ports | jq '.ports[] | select(.fixed_ips[].ip_address == "'"$GW_IP"'")' | jq '.mac_address')
GW_MAC_CLEAN=$(curl -s -H "X-Auth-Token: ${TOKEN}" http://192.168.203.210:9696/v2.0/ports | jq '.ports[] | select(.fixed_ips[].ip_address == "'"$GW_IP"'")' | jq '.mac_address' | tr -d '"')
ctx instance runtime_properties gw_mac ${GW_MAC}
ctx instance runtime_properties gw_mac_clean ${GW_MAC_CLEAN}

###################################################################
# Geting Dia instance ip address from cloudify-management-network #
# for instance configuration                                      #
###################################################################
while [[ -z "$INST_IP" ]]
do
    INST_IP=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers/${controller_id} | jq '.server.addresses."'"cloudify-management-network"'"[].addr'  | tr -d '"')
    sleep 2
done
ctx instance runtime_properties ip ${INST_IP}

###########################################
# Calculate Controller priority parameter #
###########################################
CM_PORT_IP=$(curl -s -H "X-Auth-Token: ${TOKEN_CLY}" http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers/${controller_id} | jq '.server.addresses.control_and_management_network[].addr' | tr -d '"')
if [ "$CM_PORT_IP" == "$controller1_eth0_ip" ]; then
    ctx instance runtime_properties controller_priority "A"
else 
    if [ "$CM_PORT_IP" == "$controller2_eth0_ip" ]; then
        ctx instance runtime_properties controller_priority "B"
    else
        ctx instance runtime_properties controller_priority "C"
    fi
fi