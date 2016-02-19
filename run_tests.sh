#!/usr/bin/env bash

# The cap-add and security-opt are for enabling ramdisk mounting inside the container
docker run --net=timnet --tmpfs /tmp/doctest_files:rw,noexec,nosuid,size=2m --rm -v $PWD:/service -t -i tim:$(./get_latest_date.sh) /bin/bash -c \
 "cd /service/timApp &&
 export TIM_NAME=timtest &&
 export TIM_SETTINGS=debugconfig.py &&
 python3 -c 'import dumboclient; dumboclient.launch_dumbo()' &&
 python3 -m unittest"
