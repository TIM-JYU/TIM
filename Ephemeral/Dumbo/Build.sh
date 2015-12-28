#!/bin/bash
set -exu

opts=""
if [ $# -gt 0 -a $1 = "--no-cache" ] ; then
    opts="$1"
fi

docker build -t stackage_builder -f Builder.docker $opts . 
docker run --rm -v $PWD:/build/ -w /build/ stackage_builder
