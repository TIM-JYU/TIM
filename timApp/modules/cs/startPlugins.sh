#!/bin/bash
# Restart csPlugin 

docker stop csPlugin
docker rm csPlugin

# Start csPlugin
pkill csdaemon
nohup /opt/cs/csdeamon.sh &
docker run --name csPlugin -p 56000:5000 -v /opt/cs:/cs/ -d -t -i cs3 /bin/bash -c 'cd /cs && ./run_cs3 ; /bin/bash'
