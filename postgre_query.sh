#!/usr/bin/env bash

. ./variables.sh

if [ $# -lt 1 ]; then
 echo Usage:
 echo To connect to main database: ./postgre_query 1 [sql/script]
 echo To connect to tempdb database: ./postgre_query 2 [sql/script]
 echo To connect to test database: ./postgre_query 3 [sql/script]
 echo To connect to test tempdb database: ./postgre_query 4 [sql/script]
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
  host="postgresql-tempdb"
  db="tempdb_$COMPOSE_PROJECT_NAME"
  ;;
 "3")
  host="postgresql-test"
  db="tim-test"
  ;;
 "4")
  host="postgresql-tempdb-test"
  db="tempdb_tim-test"
esac

./docker-compose.sh run -v ${PWD}/pg_backup:/backup -v ${PWD}/timApp/sql:/sql/:ro postgresql psql -h ${host} -p 5432 -d ${db} -U postgres ${fileOpt}
