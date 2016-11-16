#!/bin/bash
docker run --net=timnet --rm -v $PWD:/service -t -i timimages/funnel /bin/bash -c "cd /service && python3 -m unittest"
