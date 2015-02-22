#!/bin/bash
# see: https://github.com/docker/docker/issues/1905
set -e

to=$1
shift

starttime="`date +%Y%m%d-%H%M%S`"
cont=$(docker run -d "$@")
code=$(timeout "$to" docker wait "$cont" || true)
docker kill $cont &> /dev/null
# echo -n 'status: '
if [ -z "$code" ]; then
    echo "`date +%Y%m%d-%H%M%S` timeout: $starttime"
fi

# echo output:
# pipe to sed simply for pretty nice indentation
# docker logs $cont | sed 's/^/\t/'

docker rm $cont &> /dev/null