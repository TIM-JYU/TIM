#!/usr/bin/env bash

. ./variables.sh

TIM_NAME="${TIM_NAME}-test"

# Start PG test container if it doesn't exist yet
if [[ "$(docker ps -q --filter name=postgresql-${TIM_NAME} 2> /dev/null)" == "" ]]; then
  echo "Starting PG test container..."
  docker rm -f postgresql-${TIM_NAME} > /dev/null 2>&1
  docker run \
 --net=timnet \
 -dti \
 --name postgresql-${TIM_NAME} \
 postgres:9.5
. ./wait_for_postgre.sh postgresql-${TIM_NAME}
fi

if [[ "$(docker ps -q --filter name=postgresql-tempdb-${TIM_NAME} 2> /dev/null)" == "" ]]; then
  echo "Starting PG test container for tempdb..."
  docker rm -f postgresql-tempdb-${TIM_NAME} > /dev/null 2>&1
  docker run \
 --net=timnet \
 -dti \
 --name postgresql-tempdb-${TIM_NAME} \
 postgres:9.5
. ./wait_for_postgre.sh postgresql-tempdb-${TIM_NAME}
fi

if [[ "$(docker ps -q --filter name=${TIM_NAME}-chrome 2> /dev/null)" == "" ]]; then
 docker rm -f ${TIM_NAME}-chrome > /dev/null 2>&1
 docker run --net=timnet --name ${TIM_NAME}-chrome -d selenium/standalone-chrome
fi
