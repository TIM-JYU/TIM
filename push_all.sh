#!/bin/bash

docker push timimages/tim:$(./get_latest_date.sh)
docker push timimages/svn
docker push timimages/cs3
docker push timimages/haskelldev
docker push timimages/haskellrun
docker push timimages/pali
docker push timimages/local_nginx
docker push timimages/funnel
