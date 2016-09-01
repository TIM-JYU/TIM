#!/bin/bash
docker run --net=timnet --rm timimages/tim:$(./get_latest_date.sh) /bin/bash -c "until nc -z postgresql-$TIM_NAME 5432; do echo Waiting for postgresql-$TIM_NAME container to start up...; sleep 1; done"
echo postgresql-$TIM_NAME container is now up.
