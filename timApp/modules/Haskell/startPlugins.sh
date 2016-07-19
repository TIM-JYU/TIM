#!/bin/bash
# This is used to (re)start all Haskell plugins. Note that the ports used here must match tim registry
#set -euo pipefail
#IFS=$'\n\t'

dockername="haskellplugins2"
dockerOptions="--name $dockername --net=timnet \
   -v /opt/tim/timApp/modules/Haskell/.cabal-sandbox/bin/:/hbin\
   -v /opt/tim/timApp/modules/Haskell/:/Haskell\
   -v /opt/tim/timApp/modules/Haskell/startAll.sh:/startAll.sh\
   -p 57000:5001\
   -p 58000:5002\
   -p 59000:5003\
   -p 60000:5004\
   timimages/haskellrun  /bin/bash"


docker stop haskellplugins2 > /dev/null 2>&1
docker rm haskellplugins2 > /dev/null 2>&1

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions
else
    docker run -d $dockerOptions -c '/startAll.sh ; /bin/bash'
fi
