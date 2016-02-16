#!/bin/bash
if [ ! -f /service/funnel.py ]
then
    echo "This script is meant to be ran inside a Docker container."
    exit 1
fi

if [ ! $PWD == "/service" ]
then
    cd /service
fi

python3 funnel.py

