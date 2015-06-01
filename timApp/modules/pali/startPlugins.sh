#!/bin/bash 
# Restart pali 

docker stop pali 
docker rm pali 

sudo mkdir /var/log/pali
sudo chmod 777 /var/log/pali

# Start pali-plugin
docker run --name pali\
           -p 61000:5000\
           -v /opt/tim/timApp/modules/pali:/pali/:ro\
           -v /opt/tim/timApp/modules/py:/py/:ro\
           -v /var/log/pali:/var/log/\
           -d -t -i pali \
           /bin/bash -c 'cd /pali && ./startAll.sh ; /bin/bash'
