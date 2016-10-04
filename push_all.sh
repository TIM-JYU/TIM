#!/bin/bash
docker tag svn timimages/svn
docker tag cs3 timimages/cs3
docker tag stackage_builder timimages/stackage_builder
docker tag pali timimages/pali
docker tag funnel timimages/funnel

docker push timimages/tim:$(./get_latest_date.sh)
docker push timimages/svn
docker push timimages/cs3
docker push timimages/haskelldev
docker push timimages/haskellrun
docker push timimages/stackage_builder
docker push timimages/pali
docker push timimages/local_nginx
docker push timimages/funnel
