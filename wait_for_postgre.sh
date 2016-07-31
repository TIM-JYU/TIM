#!/bin/bash
docker run --net=timnet --rm timimages/tim:$(./get_latest_date.sh) /bin/bash -c "until nc -z postgre 5432; do echo Waiting for postgre container to start up...; sleep 1; done"
echo Postgre container is now up.
