#!/bin/bash
# Restart showFile

docker stop showFile 
docker rm showFile 

# Start showFile-plugin
docker run --name showFile -p 55000:5000 -v /opt/svn:/svn/ -d -t -i svn /bin/bash -c 'cd /svn && python3 svn3.py ; /bin/bash'
