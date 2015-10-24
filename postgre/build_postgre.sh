#!/bin/sh

CONF_DIR=/etc/postgresql/
DATA_DIR=/var/lib/postgresql/
CONTAINER_NAME=postgre_init

checkdir() {
  if [ ! -d "$1" ]; then
      echo "Folder $1 doesn't exist, copying it from container"
      docker cp $CONTAINER_NAME:$2 $1
  fi
}

docker build -t postgre .

docker run -d --name $CONTAINER_NAME -t -i postgre /bin/bash -c 'sudo -u postgres /usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf ; /bin/bash'

mkdir -p /opt/postgre/log
checkdir /opt/postgre/conf $CONF_DIR
checkdir /opt/postgre/data $DATA_DIR

cp ownership.sh /opt/postgre/conf/ownership.sh
chmod a+x /opt/postgre/conf/ownership.sh

docker stop $CONTAINER_NAME
docker rm $CONTAINER_NAME