#!/bin/bash

. ./variables.sh

if [ $# -ne "1" ]; then
 echo Usage:
 echo To connect to main database: ./postgre_query 1
 echo To connect to tempdb database: ./postgre_query 2
 exit
fi

if [ $1 = "2" ]; then
  docker run --rm --net timnet -ti postgres:9.5 /bin/bash -c "psql -h postgresql-tempdb-$TIM_NAME -p 5432 -d tempdb_$TIM_NAME -U postgres"
else
  docker run --rm --net timnet -ti postgres:9.5 /bin/bash -c "psql -h postgresql-$TIM_NAME -p 5432 -d $TIM_NAME -U postgres"
fi
