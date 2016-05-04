#!/bin/bash

# Description:
# This script just take all builded applications and
# copy then to the specifyed directory

MODULES[0]='boot'
MODULES[1]='controller/ebin'
MODULES[2]='dia/ebin'

function usage() {
cat <<HELP
Simple tool to compile all applications and install them

Use me like this:
./install.sh ~/git/diameter-relay-agent
HELP
exit
}

function compile_apps() {
    cd ${diameter_dir}/controller
    rebar compile

    cd ${diameter_dir}/dia
    rebar compile
}

function copy_diameter_apps_to_dir() {
    local to_dir="$diameter_dir/Result"
    echo "Diameter location: ${diameter_dir}"
    echo "Destination directory: ${to_dir}"

    [ -d $to_dir ] || mkdir $to_dir

    `cp ${diameter_dir}/boot/*.sh               ${to_dir}`
    `cp ${diameter_dir}/controller/ebin/*.beam  ${to_dir}`
    `cp ${diameter_dir}/dia/ebin/*.beam         ${to_dir}`
}

diameter_dir=$1
if [ ! -d "$diameter_dir" ]; then
    usage
fi

compile_apps
copy_diameter_apps_to_dir
