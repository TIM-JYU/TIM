#!/bin/bash
docker run -v $PWD:/service -t -i tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp && source initenv.sh ; python3 query.py'
