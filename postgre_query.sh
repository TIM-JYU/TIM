#!/bin/bash

docker run --rm --net=timnet -t -i postgre /bin/bash -c 'psql -h postgre -p 5432 -d docker -U docker --password'
