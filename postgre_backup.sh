#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ $# -ne 1 ]; then
 echo Usage:
 echo "./postgre_backup.sh <output directory of backup file>"
 exit
fi

if [ ! -d "$1" ]; then
 echo "$1 is not a directory."
 exit
fi

echo Backing up PostgreSQL database...
filename="$1/dump_$(date +%d-%m-%Y"_"%H_%M_%S).sql.gz"
${DIR}/docker-compose.sh exec -T postgresql pg_dumpall --clean -h localhost -p 5432 -U postgres | gzip > ${filename}
echo Done, backup saved to ${filename}.
