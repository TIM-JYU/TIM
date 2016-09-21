#!/bin/bash

. ./variables.sh
database=$TIM_NAME
container=postgresql-$TIM_NAME

echo Backing up PostgreSQL database...
docker run \
 --rm --net timnet -v ${PWD}/pg_backup:/backup -ti postgres:9.5 /bin/bash -c \
 "pg_dump -h $container -p 5432 -d $database -U postgres --jobs=1 --format=custom --file=/backup/pg_dump"
echo Done.
