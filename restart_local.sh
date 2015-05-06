#!/bin/sh

# Do NOT try to run this script in tim-beta machine!

# Stop the script if any error occurs
set -e

# Stop tim and Haskell plugins
docker stop tim &
docker stop haskellplugins2 &
wait

# Remove stopped containers
docker rm tim &
docker rm haskellplugins2 &
wait

# Start Haskell plugins
docker run -d\
   -v $PWD/timApp/modules/Haskell/.cabal-sandbox/bin/:/hbin\
   -v $PWD/timApp/modules/Haskell/:/Haskell\
   -v $PWD/timApp/modules/Haskell/startAll.sh:/startAll.sh\
   -p 57000:5001\
   -p 58000:5002\
   -p 59000:5003\
   -p 60000:5004\
   --name "haskellplugins2" haskellrun /startAll.sh

# Start tim
if [ "$1" = "sshd" ] ; then
    docker run --name tim -p 50000:5000 -p 49999:22 -v $PWD:/service -d -t -i tim /bin/bash -c '/usr/sbin/sshd -D ; /bin/bash'
    # Initialize the database in case it doesn't exist yet
    docker exec tim /bin/bash -c 'cd /service/timApp && python3 initdb2.py'
else
    docker run --name tim -p 50000:5000 -v $PWD:/service -d -t -i tim /bin/bash -c 'cd /service/timApp && python3 initdb2.py && export TIM_SETTINGS=/service/timApp/debugconfig.py && source initenv.sh ; python3 launch.py ; /bin/bash'
fi
