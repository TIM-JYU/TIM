#!/bin/sh

opts=""
if [ $# -gt 0 -a $1 = "--no-cache" ] ; then
    opts="$1"
fi

docker build --tag=local_nginx $opts .
