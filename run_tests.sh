#!/usr/bin/env bash

docker run --rm -v /opt/tim/:/service -t -i tim:$(./get_latest_date.sh) /bin/bash -c \
 "cd /service/timApp &&
 export TIM_NAME=timtest &&
 python3 -c 'import dumboclient; dumboclient.launch_dumbo()' &&
 python3 -m unittest"
