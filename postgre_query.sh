#!/bin/bash

. ./variables.sh

if [ $# -lt 1 ]; then
 echo Usage:
 echo To connect to main database: ./postgre_query 1  [sql/script]
 echo To connect to tempdb database: ./postgre_query 2 [sql/script]
 echo To connect to test database: ./postgre_query 3 [sql/script]
 exit
fi

fileOpt=''

if [ $# -ge 2 ]; then
   fileOpt=" -f /$2"
fi

if [ $1 = "2" ]; then
  database=tempdb_$TIM_NAME
elif [ $1 = "3" ]; then
  database=$TIM_NAME-test
else
  database=$TIM_NAME
fi

./docker-compose.sh run -v ${PWD}/pg_backup:/backup -v ${PWD}/timApp/sql:/sql/:ro postgresql psql -h postgresql -p 5432 -d tim -U postgres $fileOpt
