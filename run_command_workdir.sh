#!/bin/bash
# Runs the given command in the given working directory inside TIM container with the repository's root path being mapped at /service inside container.
# Usage: ./run_command_workdir.sh <working dir> <command> [arguments...]

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
 ${DOCKERFLAGS:=-t -i} \
 -w "/service/$1" \
 timimages/tim:$(${DIR}/get_latest_date.sh) \
 "${@:2}"
