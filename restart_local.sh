#!/bin/bash

# Stop the script if any error occurs
set -e

params=${*/all/tim nginx postgre plugins}

if [ "$params" = "" ] ; then
    echo "Usage: restart [tim [sshd]|nginx|plugins]..."
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
    ln -s $2 $1
  fi
}


# Create symbolic links for /opt/tim, /opt/cs and /opt/svn
checkdir /opt/tim $PWD
checkdir /opt/svn $PWD/timApp/modules/svn
checkdir /opt/cs $PWD/timApp/modules/cs
checkdir /opt/postgre $PWD/postgresql

if param tim; then
    docker stop tim &
fi

if param nginx; then
    docker stop nginx &
fi

if param postgre; then
    docker stop postgre &
fi
wait

# Remove stopped containers
if param tim; then
    docker rm tim &
fi

if param nginx; then
    docker rm nginx &
fi

if param postgre; then
    docker rm postgre &
fi
wait

if param plugins; then
 /opt/cs/startPlugins.sh
 /opt/svn/startPlugins.sh
 /opt/tim/timApp/modules/Haskell/startPlugins.sh
fi

if param postgre; then
  docker run -d --name postgre \
  -v /opt/postgre/data:/var/lib/postgresql \
  -v /opt/postgre/log:/var/log/postgresql \
  -v /opt/postgre/conf:/etc/postgresql \
  -t -i postgre /bin/bash -c '/etc/postgresql/ownership.sh && sudo -u postgres /usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf ; /bin/bash'
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
    docker run --name tim -p 50001:5000 -p 49999:22 --link postgre -v /opt/tim/:/service -d -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && source initenv.sh && export TIM_NAME=tim ; /usr/sbin/sshd -D ; /bin/bash'
  else
    docker run --name tim -p 50001:5000 --link postgre -v /opt/tim/:/service ${DAEMON_FLAG} -t -i tim:$(./get_latest_date.sh) /bin/bash -c "cd /service/timApp && source initenv.sh ; export TIM_NAME=tim ; $TIM_SETTINGS python3 launch.py --with-gunicorn $END_SHELL"
  fi
fi

if param nginx; then
  docker run -d --name nginx -p 80:80 -v /opt/cs/:/opt/cs/ -e "DOCKER_BRIDGE=$(ip ro | grep docker0 | grep -oP '(?<=src )([\d\.]+)')" local_nginx /startup.sh
fi

exit 0
