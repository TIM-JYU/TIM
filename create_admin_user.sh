#!/bin/bash
docker run --rm -v /opt/tim/:/service -t -i timimages/tim:$(./get_latest_date.sh) /bin/bash -c 'cd /service/timApp/tim_files && python3 ../sql/create_admin_user.py'
