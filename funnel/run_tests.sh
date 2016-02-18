#!/bin/bash
docker run --net=timnet --rm -v $PWD:/service -t -i funnel:$(../get_latest_date.sh) /bin/bash -c "cd /service && python3 -m unittest"
