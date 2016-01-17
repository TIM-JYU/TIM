#!/usr/bin/env bash

# The cap-add and security-opt are for enabling ramdisk mounting inside the container
docker run --cap-add SYS_ADMIN --security-opt apparmor:unconfined --rm -v $PWD:/service -t -i tim:$(./get_latest_date.sh) /bin/bash -c \
 "cd /service/timApp &&
 export TIM_NAME=timtest &&
 export TIM_SETTINGS=debugconfig.py &&
 python3 -c 'import dumboclient; dumboclient.launch_dumbo()' &&
 python3 -m unittest"
