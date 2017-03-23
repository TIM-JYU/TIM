#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

. ${DIR}/variables.sh

docker run \
 --rm \
 --net=timnet \
 --env TIM_NAME=${TIM_NAME} \
 --env TIM_HOST=${TIM_HOST} \
 --env FLASK_APP=tim_app.py \
 --env PYTHONPATH=/service/timApp \
 -v ${DIR}:/service \
 -t -i \
 -w /service/timApp \
 timimages/tim:$(${DIR}/get_latest_date.sh) \
 "$@"
