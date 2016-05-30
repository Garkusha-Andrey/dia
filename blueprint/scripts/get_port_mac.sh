#!/bin/bash -e
#############################################################
# Get token for access to the Openstack API (Admin project) #
#############################################################
if [ ! -f /home/centos/token.json ]; then
    echo -e "{\n    \"auth\": {\n        \"identity\": {\n            \"methods\": [\n                \"password\"\n            ],\n            \"password\": {\n                \"user\": {\n                    \"domain\": {\n                        \"name\": \"Default\"\n                    },\n                    \"name\": \"admin\",\n                    \"password\": \"openstack\"\n                }\n            }\n        },\n        \"scope\": {\n            \"project\": {\n                \"domain\": {\n                    \"name\": \"Default\"\n                },\n                \"name\": \"admin\"\n            }\n        }\n    }\n}" >> /home/centos/token.json
fi
TOKEN=$(curl -si -d @token.json -H "Content-type: application/json" http://192.168.203.210:5000/v3/auth/tokens | awk '/X-Subject-Token/ {print $2}')

##########################################################
# Geting mac-address of port via Openstack API           #
##########################################################
PORT_MAC=$(curl -s -H "X-Auth-Token: ${TOKEN}" http://192.168.203.210:9696/v2.0/ports/${port_id} | python -mjson.tool | awk '/mac_address/ {print $2}' | tr -d ',')
PORT_MAC_CLEAN=$(curl -s -H "X-Auth-Token: ${TOKEN}" http://192.168.203.210:9696/v2.0/ports/${port_id} | python -mjson.tool | awk '/mac_address/ {print $2}' | tr -d ',' | tr -d '"')
ctx instance runtime_properties port_mac ${PORT_MAC}
ctx instance runtime_properties port_mac_clean ${PORT_MAC_CLEAN}