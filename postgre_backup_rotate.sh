#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

${DIR}/postgre_backup.sh ${DIR}/pg_backup

# delete all backups older than a week
find ${DIR}/pg_backup/ -type f -mtime +6 -name '*.gz' -exec rm -- {} \;
