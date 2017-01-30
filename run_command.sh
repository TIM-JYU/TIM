#!/bin/bash
. ./variables.sh

docker run \
 --rm \
 --net=timnet \
 --env TIM_NAME=${TIM_NAME} \
 --env TIM_HOST=${TIM_HOST} \
 --env FLASK_APP=tim_app.py \
 --env PYTHONPATH=/service/timApp \
 -v ${PWD}:/service \
 -t -i \
 -w /service/timApp \
 timimages/tim:$(./get_latest_date.sh) \
 "$@"
