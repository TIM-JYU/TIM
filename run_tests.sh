#!/usr/bin/env bash

./start_pg_test_container.sh

docker run \
 --net=timnet \
 --tmpfs /tmp/doctest_files:rw,noexec,nosuid,size=5m \
 --rm \
 --env TIM_SETTINGS=testconfig.py \
 -v $PWD:/service:ro \
 -v $PWD/timApp/static/testgen:/service/timApp/static/testgen:rw \
 -v $PWD/timApp/static/.webassets-cache:/service/timApp/static/.webassets-cache:rw \
 -v $PWD/timApp/static/scripts:/service/timApp/static/scripts:rw \
 -t -i \
 timimages/tim:$(./get_latest_date.sh) /bin/bash -c \
 "cd /service/timApp &&
 python3 bower_helper.py &&
 python3 -c 'import dumboclient; dumboclient.launch_dumbo()' &&
 python3 -m unittest"
