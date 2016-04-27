#!/bin/bash

# Description:
# This script just take all builded applications and
# copy then to the specifyed directory

MODULES[0]='boot'
MODULES[1]='controller/ebin'
MODULES[2]='dia/ebin'

function usage() {
cat <<HELP
I'll just copy all *beam and *.sh to one directory
 
Use me like this:
./install.sh ~/git/diameter-relay-agent my_path/


HELP
}

function copy_diameter_apps_to_dir() {
    diameter_dir=$1
    to_dir=$2
    echo "Diameter location: ${diameter_dir}"
    echo "Destination directory: ${to_dir}"

    `cp ${diameter_dir}/boot/*.sh               ${to_dir}`
    `cp ${diameter_dir}/controller/ebin/*.beam  ${to_dir}`
    `cp ${diameter_dir}/dia/ebin/*.beam         ${to_dir}`
}

usage
copy_diameter_apps_to_dir $1 $2
