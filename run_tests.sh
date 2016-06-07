#!/usr/bin/env bash

docker run --net=timnet --tmpfs /tmp/doctest_files:rw,noexec,nosuid,size=2m --rm -v $PWD:/service:ro -t -i tim:$(./get_latest_date.sh) /bin/bash -c \
 "cd /service/timApp &&
 export TIM_SETTINGS=testconfig.py &&
 python3 -c 'import dumboclient; dumboclient.launch_dumbo()' &&
 python3 -m unittest"
