#!/usr/bin/env bash

. ./variables.sh

if [ $# -ne 3 ]; then
  echo Usage: ./postgre_conf.sh db_num configname value
  echo Example: ./postgre_conf.sh 1 max_connections 120
  exit
fi

case "$1" in
 "1")
  host="postgresql"
  ;;
 "2")
  host="postgresql-test"
esac

./docker-compose.sh exec ${host} bash -c \
 "sed -i -e 's/^#\{0,1\}$2 = .*$/$2 = $3/w sed_changes.txt' /var/lib/postgresql/data/postgresql.conf \
  && echo Lines changed in postgresql.conf: \
  && [ -s sed_changes.txt ] || echo \(nothing changed\) \
  && cat sed_changes.txt"
./docker-compose.sh restart ${host}
