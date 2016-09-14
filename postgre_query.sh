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
  container=postgresql-tempdb-$TIM_NAME
  database=tempdb_$TIM_NAME
elif [ $1 = "3" ]; then
  container=postgresql-timtest
  database=timtest
else
  container=postgresql-$TIM_NAME
  database=$TIM_NAME
fi
docker run --rm --net timnet -v /opt/tim/sql:/sql/:ro  -ti postgres:9.5 /bin/bash -c "psql -h $container -p 5432 -d $database -U postgres $fileOpt"
