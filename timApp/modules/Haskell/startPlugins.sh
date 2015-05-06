#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This is used to (re)start all Haskell plugins. Note that the ports used here must match tim registry

docker stop haskellplugins2
docker rm haskellplugins2

 docker run \
   -d
   -v /opt/tim/timApp/modules/Haskell/.cabal-sandbox/bin/:/hbin\
   -v /opt/tim/timApp/modules/Haskell/:/Haskell\
   -v /opt/tim/timApp/modules/Haskell/startAll.sh:/startAll.sh\
   -p 57000:5001\
   -p 58000:5002\
   -p 59000:5003\
   -p 60000:5004\
   --name "haskellplugins2" haskellrun /startAll.sh
