#!/bin/bash
docker run --net=timnet --rm -v $PWD:/service -t -i funnel /bin/bash -c "cd /service && python3 -m unittest"
