#!/bin/bash
docker tag svn timimages/svn
docker tag cs3 timimages/cs3
docker tag haskelldev timimages/haskelldev
docker tag haskellrun timimages/haskellrun
docker tag stackage_builder timimages/stackage_builder
docker tag postgre timimages/postgre
docker tag pali timimages/pali
docker tag local_nginx timimages/local_nginx
docker tag funnel timimages/funnel

docker push timimages/tim:$(./get_latest_date.sh)
docker push timimages/svn
docker push timimages/cs3
docker push timimages/haskelldev
docker push timimages/haskellrun
docker push timimages/stackage_builder
docker push timimages/postgre
docker push timimages/pali
docker push timimages/local_nginx
docker push timimages/funnel
