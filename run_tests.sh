#!/usr/bin/env bash

docker run \
 --net=timnet \
 --tmpfs /tmp/doctest_files:rw,noexec,nosuid,size=5m \
 --rm \
 -v $PWD:/service:ro \
 -v $PWD/timApp/static/testgen:/service/timApp/static/testgen:rw \
 -v $PWD/timApp/static/.webassets-cache:/service/timApp/static/.webassets-cache:rw \
 -v $PWD/timApp/static/scripts:/service/timApp/static/scripts:rw \
 -t -i \
 timimages/tim:$(./get_latest_date.sh) /bin/bash -c \
 "cd /service/timApp &&
 export TIM_SETTINGS=testconfig.py &&
 python3 bower_helper.py &&
 python3 -c 'import dumboclient; dumboclient.launch_dumbo()' &&
 python3 -m unittest"
