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

export TEST_PARAMS="discover -v tests/$1 'test_*.py' ."
if [ "$1" = "1" ] ; then
    export TEST_PARAMS="tests.$2"
fi

./docker-compose.sh up --profile test --exit-code-from tests --abort-on-container-exit tests
exitcode=$?
./docker-compose.sh down -t 0 > /dev/null 2>&1
exit ${exitcode}
