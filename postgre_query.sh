#!/usr/bin/env bash

. ./variables.sh

if [ $# -lt 1 ]; then
 echo Usage:
 echo "To connect to main database: ./postgre_query 1 [sql/<script>.sql]"
 echo "To connect to test database: ./postgre_query 2 [sql/<script>.sql]"
 exit
fi

fileOpt=''

if [ $# -ge 2 ]; then
   fileOpt=" -f /$2"
fi

case "$1" in
 "1")
  host="postgresql"
  db="$COMPOSE_PROJECT_NAME"
  ;;
 "2")
  host="postgresql-test"
  db="tim-test"
esac

./docker-compose.sh run --rm -v ${PWD}/pg_backup:/backup -v ${PWD}/sql:/sql/:ro postgresql psql -P pager=off -h ${host} -p 5432 -d ${db} -U postgres ${fileOpt}
