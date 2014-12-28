#!/bin/bash

# This script is supposed to be run on tim-beta machine.

# Stop the script if any error occurs
set -e

params="$*"

if [ "$params" = "" ] ; then
    echo "Usage: restart_all [tim|timbeta|timdev|plugins]..."
    echo "Example: restart_all tim timdev"
    exit
fi

function param {
  local list="$params"
  local item="$1"
  if [[ $list =~ (^|[[:space:]])"$item"($|[[:space:]]) ]] ; then
    result=0
  else
    result=1
  fi
  return $result
}

if param tim ; then
    docker stop tim &
fi

if param timbeta ; then
    docker stop timbeta &
fi

if param timdev ; then
    docker stop timdev &
fi
wait

# Stop plugin containers
if param plugins ; then
    docker stop csPlugin &
    docker stop showFile &
    docker stop ChoicePlug &
    docker stop ShortNotePlug &
    docker stop MultiChoicePlug &
    docker stop GraphVizPlug &
fi
wait

# Remove stopped containers
if param tim ; then
    docker rm tim &
fi

if param timbeta ; then
    docker rm timbeta &
fi

if param timdev ; then
    docker rm timdev &
fi

if param plugins ; then
    docker rm csPlugin &
    docker rm showFile &
    docker rm ChoicePlug &
    docker rm ShortNotePlug &
    docker rm MultiChoicePlug &
    docker rm GraphVizPlug &
fi
wait

if param plugins ; then
# Start csPlugin
docker run --name csPlugin -p 56000:5000 -v /opt/cs:/cs/ -d -t -i cs3 /bin/bash -c 'cd /cs && ./run_cs3 ; /bin/bash'

# Start showFile-plugin
docker run --name showFile -p 55000:5000 -v /home/vesal/svn:/svn/ -d -t -i svn /bin/bash -c 'cd /svn && python3 svn3.py ; /bin/bash'

# Start Haskell plugins
docker run --name ChoicePlug -p 57000:5000 -d -t -i haskellplugins /bin/bash -c 'cd /Choices && ./dist/build/ChoicesPlugin/ChoicesPlugin -p 5000 ; /bin/bash'

docker run --name MultiChoicePlug -p 58000:5000 -d -t -i haskellplugins /bin/bash -c 'cd /Choices && ./dist/build/MultipleChoicesPlugin/MultipleChoicesPlugin -p 5000 ; /bin/bash'

docker run --name ShortNotePlug -p 59000:5000 -d -t -i haskellplugins /bin/bash -c 'cd /Choices && ./dist/build/ShortNotePlugin/ShortNotePlugin -p 5000 ; /bin/bash'

docker run --name GraphVizPlug -p 60000:5000 -d -t -i haskellplugins /bin/bash -c 'cd /Choices && ./dist/build/GraphVizPlugin/GraphVizPlugin -p 5000 ; /bin/bash'
fi

if param timdev ; then
# Start timdev
docker run --name timdev -p 50002:5000 -v /opt/tim-dev/:/service -d -t -i tim-new /bin/bash -c 'cd /service/timApp && export TIM_SETTINGS=/service/timApp/debugconfig.py && source initenv.sh ; python3 launch.py ; /bin/bash'
fi

if param timbeta ; then
# Start timbeta
docker run --name timbeta -p 50000:5000 -v /opt/tim-beta/:/service -d -t -i tim-new /bin/bash -c 'cd /service/timApp && export TIM_SETTINGS=/service/timApp/debugconfig.py && source initenv.sh ; python3 launch.py ; /bin/bash'
fi

if param tim ; then
# Start tim
docker run --name tim -p 50001:5000 -v /opt/tim/:/service -d -t -i tim /bin/bash -c 'cd /service/timApp && source initenv.py ; python3 launch.py ; /bin/bash'
fi
