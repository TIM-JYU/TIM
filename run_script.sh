#!/bin/bash

# Runs a script from scripts directory

if [[ $# == 0 ]]
then
    echo "Available scripts to run:"
    ls -p scripts | grep -v '/$' | grep -v '^_'
else
    docker run -e "args=$*" -v $PWD:/service -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/scripts && source _initenv.sh ; python3 $args'
fi

