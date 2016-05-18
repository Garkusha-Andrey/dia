#!/bin/bash

echo "deploy servers"

function usage() {
cat <<HELP
Use me like this:
./servers_deploy.sh ~/git/diameter-relay-agent/Result service_config
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
    IP=$(echo $line | cut -d "," -f 3)
    Port=$(echo $line | cut -d "," -f 4)
    NodeName=$(echo $HostID | cut -d "." -f 1)

    echo "Realm: $RealmID, Host: $HostID, IP: $IP, Port: $Port"
    #run_cur_host_xterm
    local local_deploy="-s server deploy '$NodeName' '$RealmID' $IP $Port"
    ERL_COMM="$ERL_COMM $local_deploy"

  done < "$servers_config"

  START_COMM="erl -sname serv $ERL_COMM"
  eval $START_COMM
}

# main
bin_dir=$1
srv_bin=$1/server.beam
config_file=$2
if [ ! -f "$srv_bin" ] || [ ! -f $config_file ]; then
    if [ ! -f "$srv_bin" ]; then
        echo "didn't find server.beam in $srv_bin"
    fi
    if [ ! -f $config_file ]; then 
        echo "didn't find config file"
    fi
    usage
fi

cd $bin_dir
start $config_file

