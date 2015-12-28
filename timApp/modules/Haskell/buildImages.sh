#!/bin/sh
set -eu pipefail
IFS=$'\n\t'

opts=""
if [ $# -gt 0 -a $1 = "--no-cache" ] ; then
    opts="$1"
fi

# This build the development environment and the run environment images

docker build -t "haskelldev" $opts .
docker build -t "haskellrun" $opts runEnvironment/

