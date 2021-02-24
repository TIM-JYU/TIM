#!/usr/bin/env bash
docker run  --name cs --rm=true  -t -i -v /opt/tim/timApp/modules/cs:/cs -v /var/run/docker.sock:/var/run/docker.sock -v /opt/tim/timApp/modules/cs/generated:/csgenerated -v /opt/tim/timApp/modules/cs/static:/csstatic:ro -v /opt/tim/tim_common:/cs/tim_common:ro -v /tmp/${COMPOSE_PROJECT_NAME}_uhome:/tmp timimages/cs3:compose
