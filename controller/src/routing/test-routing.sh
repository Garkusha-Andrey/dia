#!/bin/bash

TMPDIR="tmp.test"

if ! [ -e routing.erl ]
then
    echo "Please start from routing/ directory"
    exit 1
fi

if [ -e $TMPDIR ]
then
    echo "$TMPDIR already exists"
    exit 1
fi

mkdir $TMPDIR
cp *.erl $TMPDIR
cp stubs/controller_lib.erl.TEST $TMPDIR/controller_lib.erl
cp stubs/restconf.erl.TEST $TMPDIR/restconf.erl
cp stubs/rlog.erl.TEST $TMPDIR/rlog.erl
ln -s ../controller_app.hrl

cd $TMPDIR
erlc *.erl

erl -eval "routing:init(), \
routing:update(), \
routing:update(), \
routing:update(), \
routing:update(), \
init:stop()"

cd ..
rm -rf $TMPDIR
if [ -L controller_app.hrl ] ; then rm controller_app.hrl; fi
