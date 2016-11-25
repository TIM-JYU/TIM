#!/usr/bin/env bash

# Usage:
# ./run_tests.sh          # Runs all tests
# ./run_tests.sh unit     # Runs unit tests
# ./run_tests.sh db       # Runs db tests
# ./run_tests.sh server   # Runs server tests
# ./run_tests.sh browser  # Runs browser tests
# ./run_tests.sh 1 unit.test_attributeparser.AttributeParserTest.test_random  # Runs a single test method
# ./run_tests.sh 1 unit.test_attributeparser.AttributeParserTest              # Runs all tests in a class
# ./run_tests.sh 1 unit.test_attributeparser                                  # Runs all tests in a module

./start_pg_test_containers.sh

. ./variables.sh

params="discover tests/$1 'test_*.py' ."
if [ "$1" = "1" ] ; then
    params="tests.$2"
fi

docker run \
 --net=timnet \
 --tmpfs /tmp/doctest_files:rw,noexec,nosuid,size=5m \
 --rm \
 --name=${TIM_NAME}-test \
 --env TIM_NAME=${TIM_NAME} \
 --env TIM_SETTINGS=testconfig.py \
 -v $PWD:/service:ro \
 -v $PWD/timApp/static/testgen:/service/timApp/static/testgen:rw \
 -v $PWD/timApp/static/.webassets-cache:/service/timApp/static/.webassets-cache:rw \
 -v $PWD/timApp/static/scripts:/service/timApp/static/scripts:rw \
 -t -i \
 timimages/tim:$(./get_latest_date.sh) /bin/bash -c \
 "cd /service/timApp &&
 python3 bower_helper.py &&
 python3 -m unittest ${params}"
