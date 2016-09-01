#!/bin/bash

# This script is supposed to be run on tim-beta machine.

# Stop the script if any error occurs
set -e
#trap 'echo ABORTED on line \"$BASH_COMMAND\"' 0

if [ ! -f ./variables.sh ]; then
  echo "The file variables.sh does not exist, copying template. See variables.sh for how to proceed."
  cp variables.sh.template variables.sh
  chmod u+x variables.sh
  exit 1
fi
. ./variables.sh

params=${*/all/tim postgre plugins funnel}

if [ "$params" = "" ] ; then
    echo "Usage: restart [tim|plugins|postgre|funnel|nginx|sshd]..."
    echo "Example: restart tim plugins"
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

if [ "$CREATE_SYMLINKS" = true ]; then
checkdir /opt/tim $PWD
checkdir /opt/svn $PWD/timApp/modules/svn
checkdir /opt/cs $PWD/timApp/modules/cs
checkdir /opt/funnel $PWD/funnel
fi

if [ "$USE_FUNNEL" = true ]; then
checkdir /var/log/funnel $PWD/tim_logs
fi

if [ "$USE_WUFF" = true ]; then
checkdir /var/log/wuff $PWD/tim_logs
# Set a lock for the watchdog
touch ${PWD}/restarting
fi

if param tim ; then
    docker stop ${TIM_NAME} > /dev/null 2>&1 &
fi

if param postgre ; then
    docker stop postgre-${TIM_NAME} > /dev/null 2>&1 &
fi

if [ "$USE_FUNNEL" = true ] && param funnel ; then
    docker stop funnel > /dev/null 2>&1 &
fi
wait

# Remove stopped containers
if param tim ; then
    docker rm ${TIM_NAME} > /dev/null 2>&1 &
fi

if param postgre ; then
    docker rm postgresql-${TIM_NAME} > /dev/null 2>&1 &
fi

if [ "$USE_FUNNEL" = true ] && param funnel ; then
    docker rm funnel > /dev/null 2>&1 &
fi
wait

if param plugins ; then
 ./start_plugins.sh
fi

TIM_SETTINGS=''
END_SHELL='; /bin/bash'
DAEMON_FLAG='-d'
LAUNCH_COMMAND='python3 launch.py --with-gunicorn'
if param debug ; then
  TIM_SETTINGS='TIM_SETTINGS=debugconfig.py'
  LAUNCH_COMMAND='python3 launch.py'
  END_SHELL=''
  DAEMON_FLAG=''
fi
if param profile ; then
  TIM_SETTINGS='TIM_SETTINGS=profileconfig.py'
  LAUNCH_COMMAND='python3 launch.py'
  END_SHELL=''
  DAEMON_FLAG=''
fi
SSHD_FLAGS=''
if param sshd ; then
  ./start_pg_test_container.sh
  SSHD_FLAGS='--tmpfs /tmp/doctest_files:rw,noexec,nosuid,size=2m -p 49999:22'
  LAUNCH_COMMAND='/usr/sbin/sshd -D'
fi

if [ "$USE_FUNNEL" = true ] && param funnel ; then
docker run --net=timnet -dti --name funnel \
    -v /opt/funnel:/service \
    -v /opt/tim/tim_logs:/var/log/funnel \
    funnel /service/run.sh
fi

PG_PORT=''
if [ "$OPEN_PG_PORT" = true ]; then
  PG_PORT='-p 5432:5432'
fi

if param postgre ; then
  docker volume create --name ${TIM_NAME}_data
  docker run --net=timnet -d --name postgresql-${TIM_NAME} \
  -v ${TIM_NAME}_data:/var/lib/postgresql/data \
  ${PG_PORT} \
  -t -i postgres:9.5
  . ./wait_for_postgre.sh
fi

if param tim ; then
# Start tim
docker run \
 --net=timnet \
 --name ${TIM_NAME} \
 --env TIM_NAME=${TIM_NAME} \
 --env TIM_HOST=${TIM_HOST} \
 --env TIM_SETTINGS=${TIM_SETTINGS} \
 ${SSHD_FLAGS} \
 -p ${TIM_PORT}:5000 \
 -v ${PWD}:/service \
 ${DAEMON_FLAG} -t -i \
 timimages/tim:$(./get_latest_date.sh) \
 /bin/bash -c "cd /service/timApp && source /service/scripts/_initenv.sh ; $LAUNCH_COMMAND $END_SHELL"
fi

if param nginx; then
  docker run --net=timnet -d --name nginx -p 80:80 -v /opt/cs/:/opt/cs/ local_nginx /startup.sh
fi

if [ "$USE_WUFF" = true ] && param wuff ; then
# (Re)start the watchdog
wuffpid=$(ps a | grep 'python3 wuff' | grep -v grep | awk -F" " '{ print $1 }')
if [ ! -z "$wuffpid" ] ; then
sudo kill $wuffpid
fi
#sudo kill $(ps a | grep 'python3 wuff' | grep -v grep | awk -F" " '{ print $1 }') 2> /dev/null
screen -dmS wuff sudo python3 wuff.py tim
fi

if [ "$USE_WUFF" = true ]; then
# Remove the lock
rm ${PWD}/restarting
fi

#trap '' 0
exit 0
