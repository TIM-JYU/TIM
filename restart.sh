#!/bin/bash

# This script is supposed to be run on tim-beta machine.

# Stop the script if any error occurs
set -e
#trap 'echo ABORTED on line \"$BASH_COMMAND\"' 0

params="$*"

if [ "$params" = "" ] ; then
    echo "Usage: restart [tim|timbeta|timdev|plugins]..."
    echo "Example: restart tim timdev"
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
wait

if param plugins ; then
 /opt/cs/startPlugins.sh
 /opt/svn/startPlugins.sh
 /opt/tim/timApp/modules/Haskell/startPlugins.sh
fi

if param timdev ; then
# Start timdev
docker run --name timdev -p 50002:5000 -v /opt/tim-dev/:/service -d -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && export TIM_SETTINGS=/service/timApp/debugconfig.py && source initenv.sh ; python3 launch.py --with-gunicorn ; /bin/bash'
fi

if param timbeta ; then
# Start timbeta
docker run --name timbeta -p 50000:5000 -v /opt/tim-beta/:/service -d -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && export TIM_SETTINGS=/service/timApp/debugconfig.py && source initenv.sh ; python3 launch.py --with-gunicorn ; /bin/bash'
fi

if param tim ; then
# Start tim
docker run --name tim -p 50001:5000 -v /opt/tim/:/service -d -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && source initenv.py ; python3 launch.py --with-gunicorn ; /bin/bash'
fi

#trap '' 0
exit 0
