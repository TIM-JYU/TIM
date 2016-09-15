#!/bin/bash
docker run --net=timnet --rm -v /opt/tim/:/service -t -i timimages/tim:$(./get_latest_date.sh) \
  /bin/bash -c 'cd /service/timApp && PYTHONPATH=PYTHONPATH:/service/timApp python3 sql/create_admin_user.py'
