#!/bin/bash

echo "deploy servers"

function usage() {
cat <<HELP
Use me like this:
./client_deploy.sh ~/git/diameter-relay-agent/Result clients.conf
HELP
exit
}

function start {
  servers_config=$1
  while read -r real_line; do

    if [[ $real_line != "{"* ]]; then
        continue
    fi

    line=$(echo $real_line | tr -d '{' | tr -d ' ' | tr -d '}')

    HostID=$(echo $line | cut -d "," -f 1 | tr -d '"')
    RealmID=$(echo $line | cut -d "," -f 2 | tr -d '"')
    LIP=$(echo $line | cut -d "," -f 3)
    RIP=$(echo $line | cut -d "," -f 4)
    Port=$(echo $line | cut -d "," -f 5)
    NodeName=$(echo $HostID | cut -d "." -f 1)

    echo "Realm: $RealmID, Host: $HostID, local IP: $LIP, server IP: $RIP Port: $Port"
    local local_deploy="-s client deploy '$NodeName' '$RealmID' $LIP $RIP $Port"
    ERL_COMM="$ERL_COMM $local_deploy"

  done < "$servers_config"

  ERL_COMM="-s client_deploy start $ERL_COMM"

  START_COMM="erl -sname cli $ERL_COMM"
  eval $START_COMM
}

# main
bin_dir=$1
srv_bin=$1/client.beam
config_file=`readlink -f $2`
if [ ! -f "$srv_bin" ] || [ ! -f $config_file ]; then
    if [ ! -f "$srv_bin" ]; then
        echo "didn't find client.beam in $srv_bin"
    fi
    if [ ! -f $config_file ]; then 
        echo "didn't find config file"
    fi
    usage
fi

cd $bin_dir
start $config_file

