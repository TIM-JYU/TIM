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

if param postgre ; then
    docker stop postgre &
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

if param postgre ; then
    docker rm postgre &
fi
wait

if param plugins ; then
 /opt/cs/startPlugins.sh
 /opt/svn/startPlugins.sh
 /opt/tim/timApp/modules/Haskell/startPlugins.sh
fi

TIM_SETTINGS=''
END_SHELL='; /bin/bash'
DAEMON_FLAG='-d'
if param debug ; then
  TIM_SETTINGS='TIM_SETTINGS=/service/timApp/debugconfig.py'
  END_SHELL=''
  DAEMON_FLAG=''
fi
if param profile ; then
  TIM_SETTINGS='TIM_SETTINGS=/service/timApp/profileconfig.py'
  END_SHELL=''
  DAEMON_FLAG=''
fi

if param postgre ; then
# Restart postgre container
docker run -d --name postgre \
    -v /opt/postgre/data:/var/lib/postgresql \
    -v /opt/postgre/log:/var/log/postgresql \
    -v /opt/postgre/conf:/etc/postgresql \
    -t -i postgre /bin/bash -c '/etc/postgresql/ownership.sh && sudo -u postgres /usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf ; /bin/bash'
fi

if param timdev ; then
# Start timdev
docker run --name timdev -p 50002:5000 -v /opt/tim-dev/:/service ${DAEMON_FLAG} -t -i tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source initenv.sh ; $TIM_SETTINGS python3 launch.py --with-gunicorn $END_SHELL"
fi

if param timbeta ; then
# Start timbeta
docker run --name timbeta -p 50000:5000 -v /opt/tim-beta/:/service ${DAEMON_FLAG} -t -i tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source initenv.sh ; TIM_NAME=timbeta ; $TIM_SETTINGS python3 launch.py --with-gunicorn $END_SHELL"
fi

if param tim ; then
# Start tim
docker run --name tim -p 50001:5000 --cpuset=0,0 -v /opt/tim/:/service ${DAEMON_FLAG} -t -i tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source initenv.sh ; $TIM_SETTINGS python3 launch.py --with-gunicorn $END_SHELL"
fi

#trap '' 0
exit 0
