#!/bin/bash
. ./variables.sh

docker run \
 --rm \
 --net=timnet \
 --env TIM_NAME=${TIM_NAME} \
 --env TIM_HOST=${TIM_HOST} \
 --env FLASK_APP=tim_app.py \
 -v ${PWD}:/service \
 -t -i \
 timimages/tim:$(./get_latest_date.sh) \
 /bin/bash -c "cd /service/timApp && PYTHONPATH=\${PYTHONPATH}:/service/timApp $*"
