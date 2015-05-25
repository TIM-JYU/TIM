#!/bin/bash

# Stop the script if any error occurs
set -e

params="$*"

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

if param tim ; then
    docker stop tim &
fi

if param nginx ; then
    docker stop nginx &
fi
wait

# Remove stopped containers
if param tim ; then
    docker rm tim &
fi

if param nginx ; then
    docker rm nginx &
fi
wait

if param plugins ; then
 /opt/cs/startPlugins.sh
 /opt/svn/startPlugins.sh
 /opt/tim/timApp/modules/Haskell/startPlugins.sh
fi

if param tim ; then
  if param sshd ; then
    docker run --name tim -p 50001:5000 -p 49999:22 -v /opt/tim:/service -d -t -i tim:$(./get_latest_date.sh) /bin/bash -c '/usr/sbin/sshd -D ; /bin/bash'
  else
    docker run --name tim -p 50001:5000 -v /opt/tim/:/service -d -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && source initenv.sh ; python3 launch.py ; /bin/bash'
  fi
fi

if param nginx ; then
  docker run -d --name nginx -p 80:80 -v /opt/cs/:/opt/cs/ local_nginx
fi
exit 0
