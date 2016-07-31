#!/bin/bash

# This script is supposed to be run on tim-beta machine.

# Stop the script if any error occurs
set -e
#trap 'echo ABORTED on line \"$BASH_COMMAND\"' 0

params=${*/all/tim postgre plugins funnel}

if [ "$params" = "" ] ; then
    echo "Usage: restart [tim|timbeta|timdev|plugins|postgre|funnel]..."
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

checkdir() {
  if [ ! -d "$1" ] && [ ! -L "$1" ]; then
    echo "File $1 doesn't exist, creating symbolic link"
    sudo ln -s $2 $1
  fi
}

checkdir /var/log/funnel $PWD/tim_logs
checkdir /var/log/wuff $PWD/tim_logs

# Set a lock for the watchdog
touch /opt/tim/restarting

if param tim ; then
    docker stop tim > /dev/null 2>&1 &
fi

if param timbeta ; then
    docker stop timbeta > /dev/null 2>&1 &
fi

if param timdev ; then
    docker stop timdev > /dev/null 2>&1 &
fi

if param postgre ; then
    docker stop postgre > /dev/null 2>&1 &
fi

if param funnel; then
    docker stop funnel > /dev/null 2>&1 &
fi
wait

# Remove stopped containers
if param tim ; then
    docker rm tim > /dev/null 2>&1 &
fi

if param timbeta ; then
    docker rm timbeta > /dev/null 2>&1 &
fi

if param timdev ; then
    docker rm timdev > /dev/null 2>&1 &
fi

if param postgre ; then
    docker rm postgre > /dev/null 2>&1 &
fi

if param funnel; then
    docker rm funnel > /dev/null 2>&1 &
fi
wait

if param plugins ; then
 ./start_plugins.sh
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

if param funnel; then
docker run --net=timnet -dti --name funnel \
    -v /opt/funnel:/service \
    -v /opt/tim/tim_logs:/var/log/funnel \
    funnel /service/run.sh
fi

if param postgre ; then
# Restart postgre container
docker run --net=timnet -d --name postgre \
    -v /opt/postgre/data:/var/lib/postgresql \
    -v /opt/postgre/log:/var/log/postgresql \
    -v /opt/postgre/conf:/etc/postgresql \
    -t -i postgre /bin/bash -c '/etc/postgresql/ownership.sh && sudo -u postgres /usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf ; /bin/bash'
    ./wait_for_postgre.sh
fi

if param timdev ; then
# Start timdev
docker run --net=timnet --name timdev -p 50002:5000 -v /opt/tim-dev/:/service ${DAEMON_FLAG} -t -i timimages/tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source /service/scripts/_initenv.sh ; export TIM_NAME=timdev ; export TIM_HOST=https://tim-dev.it.jyu.fi ; $TIM_SETTINGS python3 launch.py --with-gunicorn $END_SHELL"
fi

if param timbeta ; then
# Start timbeta
docker run --net=timnet --name timbeta -p 50000:5000 -v /opt/tim-beta/:/service ${DAEMON_FLAG} -t -i timimages/tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source /service/scripts/_initenv.sh ; export TIM_NAME=timbeta ; export TIM_HOST=https://tim-beta.it.jyu.fi ; $TIM_SETTINGS python3 launch.py --with-gunicorn $END_SHELL"
fi

if param tim ; then
# Start tim
docker run --net=timnet --name tim -p 50001:5000  -v /opt/tim/:/service ${DAEMON_FLAG} -t -i timimages/tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source /service/scripts/_initenv.sh ; export TIM_NAME=tim ; export TIM_HOST=https://tim.jyu.fi ; $TIM_SETTINGS python3 launch.py --with-gunicorn $END_SHELL"
fi

if param wuff ; then
# (Re)start the watchdog
wuffpid=$(ps a | grep 'python3 wuff' | grep -v grep | awk -F" " '{ print $1 }')
if [ ! -z "$wuffpid" ] ; then
sudo kill $wuffpid
fi
#sudo kill $(ps a | grep 'python3 wuff' | grep -v grep | awk -F" " '{ print $1 }') 2> /dev/null
screen -dmS wuff sudo python3 wuff.py tim
fi

# Remove the lock
rm /opt/tim/restarting

#trap '' 0
exit 0
