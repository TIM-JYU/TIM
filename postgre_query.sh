#!/bin/bash

docker run --rm --link postgre -t -i postgre /bin/bash -c 'psql -h postgre -p 5432 -d docker -U docker --password'
