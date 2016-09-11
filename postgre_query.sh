#!/bin/bash

. ./variables.sh

if [ $# -lt 1 ]; then
 echo Usage:
 echo To connect to main database: ./postgre_query 1  [sql/script]
 echo To connect to tempdb database: ./postgre_query 2 [sql/script]
 exit
fi

fileOpt=''

if [ $# -ge 2 ]; then
   fileOpt=" -f /$2"
fi

if [ $1 = "2" ]; then
  docker run --rm --net timnet -v /opt/tim/sql:/sql/:ro  -ti postgres:9.5 /bin/bash -c "psql -h postgresql-tempdb-$TIM_NAME -p 5432 -d tempdb_$TIM_NAME -U postgres $fileOpt"
else
  docker run --rm --net timnet -v /opt/tim/sql:/sql/:ro  -ti postgres:9.5 /bin/bash -c "psql -h postgresql-$TIM_NAME        -p 5432 -d        $TIM_NAME -U postgres $fileOpt"
fi
