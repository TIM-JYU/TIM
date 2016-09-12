#!/bin/bash

. ./variables.sh

if [ $# -ne 3 ]; then
  echo Usage: ./postgre_conf.sh db_num configname value
  echo Example: ./postgre_conf.sh 1 max_connections 120
  exit
fi

if [ $1 = "2" ]; then
  container=postgresql-tempdb-${TIM_NAME}
elif [ $1 = "3" ]; then
  container=postgresql-timtest
else
  container=postgresql-${TIM_NAME}
fi

docker exec ${container} bash -c \
 "sed -i -e 's/^#\{0,1\}$2 = .*$/$2 = $3/w sed_changes.txt' /var/lib/postgresql/data/postgresql.conf \
  && echo Lines changed in postgresql.conf: \
  && [ -s sed_changes.txt ] || echo \(nothing changed\) \
  && cat sed_changes.txt"
echo Restarting ${container}...
docker restart ${container}
