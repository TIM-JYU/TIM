#!/usr/bin/env bash

# Execute mongosh as user while creating a database if it does not exist
# Arguments:
#   $1: database host
#   $2: user name
#   $3: query to execute

db_host=$1;
user_name=$2;
user_password="${user_name}_pw";
user_db="${user_name}_db";
query_contents="$(cat "$3")";

user_script="
use admin;
if (!db.getUser('${user_name}')) {
  db.createUser({
    user: '${user_name}',
    pwd: '${user_password}',
    roles: [ { role: 'dbAdmin', db: '${user_db}' },
             { role: 'readWrite', db: '${user_db}' }]
  });
}
";

# First, run mongosh as superuser and create user if it doesn't exist yet
mongosh \
  --quiet \
  --norc \
  --username "mongo" \
  --password "mongodb" \
  --eval "$user_script" \
  --authenticationDatabase "admin" \
  "$db_host"

# Then, run mongosh as the user
mongosh \
  --quiet \
  --norc \
  --username "$user_name" \
  --password "$user_password" \
  --eval "$query_contents" \
  --authenticationDatabase "admin" \
  "$db_host/$user_db"