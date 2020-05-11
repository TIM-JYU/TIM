#!/usr/bin/env bash

if [ $# -ne 1 ]; then
 echo Usage:
 echo "./postgre_restore.sh <dumpfile.sql.gz>"
 exit
fi

echo "WARNING! You are about to restore PostgreSQL database from file $1. The current database will be overwritten."
echo -n "Type 'yes' to continue or any other string to quit: "
read answer
if [ "$answer" != "yes" ]; then
 echo "Aborted."
 exit
fi

echo Restoring PostgreSQL database...
zcat "$1" | docker exec -i $(./docker-compose.sh ps -q postgresql) psql -h localhost -p 5432 -U postgres
echo Done.
