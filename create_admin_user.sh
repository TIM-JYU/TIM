#!/bin/bash
. ./variables.sh

docker run \
 --net=timnet \
 --env TIM_NAME=${TIM_NAME} \
 --env TIM_HOST=${TIM_HOST} \
 -v ${PWD}:/service \
 -t -i \
 timimages/tim:$(./get_latest_date.sh) \
 /bin/bash -c 'cd /service/timApp && PYTHONPATH=${PYTHONPATH}:/service/timApp python3 sql/create_admin_user.py'
