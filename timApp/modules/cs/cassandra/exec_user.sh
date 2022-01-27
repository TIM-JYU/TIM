#!/usr/bin/env bash

# Execute cqlsh as a user on an isolated keyspace. User is created if it does not exist.
# Arguments:
#   $1: database host
#   $2: user name
#   $3: user password
#   $4: true/false whether to drop user keyspace before executing query
#   $5: query file to execute

db_host=$1;
user_name=$2;
user_password=$3;
drop_keyspace=$4;
query_file=$5;
user_keyspace="${user_name}_db";

# if drop_keyspace is true, add DROP KEYSPACE IF EXISTS ${user_keyspace}; to query string
if [ "$drop_keyspace" = true ]; then
    drop_string="DROP KEYSPACE IF EXISTS ${user_keyspace};";
else
    drop_string="";
fi

query_script="
CREATE ROLE IF NOT EXISTS ${user_name} WITH PASSWORD = '${user_password}' AND SUPERUSER = false AND LOGIN = true;
${drop_string}
CREATE KEYSPACE IF NOT EXISTS ${user_keyspace} WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1};
GRANT ALL ON KEYSPACE ${user_keyspace} TO ${user_name};
REVOKE AUTHORIZE ON KEYSPACE ${user_keyspace} FROM ${user_name};

USE ${user_keyspace};
LOGIN ${user_name} '${user_password}';
$(cat "${query_file}")";

cqlsh \
  -u cassandra \
  -p cassandra \
  -e "${query_script}" \
  "${db_host}"