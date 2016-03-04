#!/bin/bash
docker run -e "args=$1" -v $PWD:/service -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && source initenv.sh ; python3 query.py $args'
