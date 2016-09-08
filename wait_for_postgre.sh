#!/bin/bash
docker run --net=timnet --rm timimages/tim:$(./get_latest_date.sh) /bin/bash -c "until nc -z $1 5432; do echo Waiting for $1 container to start up...; sleep 1; done"
echo $1 container is now up.
