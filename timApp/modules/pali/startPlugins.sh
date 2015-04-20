#!/bin/bash
# Restart showFile

docker stop pali 
docker rm pali 

# Start showFile-plugin
docker run --name pali -p 61000:5000 -v /opt/tim/timApp/modules/pali:/pali/ -d -t -i pali /bin/bash -c 'cd /pali && python3 pali.py ; /bin/bash'
