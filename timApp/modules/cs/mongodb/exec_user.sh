#!/usr/bin/env bash

# Execute mongosh as a user on an isolated database. User is created if it does not exist.
# Arguments:
#   $1: database host
#   $2: user name
#   $3: user password
#   $4: true/false whether to drop user database before executing query
#   $5: query file to execute

db_host=$1;
user_name=$2;
user_password=$3;
drop_database=$4;
user_db="${user_name}_db";

user_create_script="
let adminDb = db.getSiblingDB('admin');
if (!adminDb.getUser('${user_name}')) {
  adminDb.createUser({
    user: '${user_name}',
    pwd: '${user_password}',
    roles: [ { role: 'dbAdmin', db: '${user_db}' },
             { role: 'readWrite', db: '${user_db}' }]
  });
}
if (${drop_database}) {
  db.dropDatabase();
}
adminDb.auth('${user_name}', '${user_password}');
delete db.auth;
delete db.logout;
delete db.changeUserPassword;
delete db.updateUser;
delete db.getSiblingDB;
delete db.shutdownServer;
delete db.adminCommand;
delete db.runCommand;
delete global.Mongo;
delete global.connect;
delete global.sleep;
delete global.adminDb;

$(cat "$5")
";

## First, run mongosh as superuser and create user if it doesn't exist yet
mongosh \
  --quiet \
  --norc \
  --username "mongo" \
  --password "mongodb" \
  --eval "$user_create_script" \
  --authenticationDatabase "admin" \
  "mongodb://$db_host/$user_db";
