#!/bin/bash

# Stop the script if any error occurs
set -e

params=${*/all/tim nginx postgre plugins funnel}

if [ "$params" = "" ] ; then
    echo "Usage: restart [tim [sshd]|nginx|plugins|postgre|funnel]..."
    echo "Example: restart tim plugins"
    exit
fi

param () {
  local list="$params"
  local item="$1"
  if echo "$list" | grep -q "\b$item\b"; then
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


# Create symbolic links for /opt/tim, /opt/cs and /opt/svn
checkdir /opt/tim $PWD
checkdir /opt/svn $PWD/timApp/modules/svn
checkdir /opt/cs $PWD/timApp/modules/cs
checkdir /opt/funnel $PWD/funnel
checkdir /var/log/funnel $PWD/tim_logs
checkdir /var/log/wuff $PWD/tim_logs

# Set a lock for the watchdog
touch /opt/tim/restarting

# Stop and remove containers
if param tim; then
    docker rm -f tim > /dev/null 2>&1 &
fi

if param nginx; then
    docker rm -f nginx > /dev/null 2>&1 &
fi

if param postgre; then
    docker rm -f postgre > /dev/null 2>&1 &
fi

if param funnel; then
    docker rm -f funnel > /dev/null 2>&1 &
fi
wait

if param funnel; then
    docker run --net=timnet -dti --name funnel \
    -v /opt/funnel:/service \
    -v /opt/tim/tim_logs:/var/log/funnel \
    funnel /service/run_local.sh
fi

if param plugins; then
 ./start_plugins.sh
fi

if param postgre; then
  docker run --net=timnet -d --name postgre \
  -v /opt/postgre/data:/var/lib/postgresql/9.3 \
  -v /opt/postgre/log:/var/log/postgresql \
  -v /opt/postgre/conf:/etc/postgresql/9.3 \
  -t -i postgre /bin/bash -c '/etc/postgresql/9.3/ownership.sh && sudo -u postgres /usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf ; /bin/bash'
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

if param tim; then
  if param sshd ; then
    docker run --net=timnet --name tim -p 50001:5000 -p 49999:22 -v /opt/tim/:/service -d -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && source initenv.sh && export TIM_NAME=tim ; export TIM_HOST=localhost ; /usr/sbin/sshd -D ; /bin/bash'
  else
    docker run --net=timnet --name tim -p 50001:5000 -v /opt/tim/:/service ${DAEMON_FLAG} -t -i tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source initenv.sh ; export TIM_NAME=tim ; export TIM_HOST=localhost ; $TIM_SETTINGS python3 launch.py $END_SHELL"
  fi
fi

if param nginx; then
  docker run --net=timnet -d --name nginx -p 80:80 -v /opt/cs/:/opt/cs/ local_nginx /startup.sh
fi

# Remove the lock
rm /opt/tim/restarting

exit 0
