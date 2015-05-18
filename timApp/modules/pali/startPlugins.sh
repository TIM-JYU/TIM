#!/bin/bash
# Restart pali 

docker stop pali 
docker rm pali 

sudo mkdir log
sudo chmod 777 log

# Start pali-plugin
docker run --name pali\
           -p 61000:5000\
           -v /opt/tim/timApp/modules/pali:/pali/:ro\
           -d -t -i pali \
           /bin/bash -c 'cd /pali && python3 pali.py ; /bin/bash'
