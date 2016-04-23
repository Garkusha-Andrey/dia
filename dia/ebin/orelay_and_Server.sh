#!/bin/bash

echo "Start orelay and server locally"

function start {
    start_remote_server;
    sleep 1;
    start_orelay;
}

function start_orelay {
    xterm -hold -e "echo ORelay;\
                    erl -sname 'or1' -s orelay deploy or1 'ex.com' '{127,0,0,1}' '{127,0,0,1}' 3911" &
}

function start_remote_server {
    xterm -hold -e "echo Remote server;\
                    erl -sname 's1' -s server deploy s1 'ex.com' '{127,0,0,1}' 3911" &
}


# main
start $1

