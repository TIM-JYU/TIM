#!/bin/bash

. ./variables.sh

docker run --rm --net timnet -ti postgres:9.5 /bin/bash -c "psql -h postgresql-$TIM_NAME -p 5432 -d $TIM_NAME -U postgres"
