#!/usr/bin/env bash

. ./variables.sh

echo Backing up PostgreSQL database...
./docker-compose.sh run -v ${PWD}/pg_backup:/backup postgresql /bin/bash -c \
 "pg_dump -h postgresql -p 5432 -d $COMPOSE_PROJECT_NAME -U postgres --jobs=1 --format=custom --file=/backup/pg_dump"
echo Done.
