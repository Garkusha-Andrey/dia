#!/usr/bin/python
import sys
import os
import subprocess

ODL_PATH="/home/nfvpilot/opendaylight/distribution/distributions/karaf/target"
CONF_SCRIPT="configure_cluster.sh"
RUN_SCRIPT="start"

def usage():
    print "Usage: ./setup_odl.py ODL_ADDRESS_1 ODL_ADDRESS_2 ODL_ADDRESS_3"

i=0
me=0
ips=""
for arg in sys.argv[1:]:
    i+=1
    ips += " "+arg
    if subprocess.call("ifconfig | grep \"inet addr:" + arg + " \" >/dev/null",
                       shell = True) == 0:
        me = i

if i<3:
    print "Warning: It's recommended to use 3 controllers"

if me == 0:
    print "Error: No local address among the provided"
    usage()
    exit(1)

script = subprocess.check_output("find " + ODL_PATH + " -name " + CONF_SCRIPT,
                                 shell = True)

print "Starting: " + script[:-1] + " " + str(me) + ips
subprocess.call(script[:-1] + " " + str(me) + ips, shell = True)

script = subprocess.check_output("find " + ODL_PATH + " -name " + RUN_SCRIPT,
                                 shell = True)

print "Starting ODL: " + script[:-1]
subprocess.call(script[:-1])
