#!/bin/bash
# Restart showFile

docker stop showFile
docker rm showFile

# Start showFile-plugin
if [ "$1" = "i" ]
then
    # interactive
    docker run  --name showFile --rm=true  -t -i -p 55000:5000 -v /opt/svn:/svn/:ro -w /svn svn /bin/bash 
else
    docker run --name showFile -p 55000:5000 -v /opt/svn:/svn/ -d -t -i svn /bin/bash -c './startAll.sh ; /bin/bash' 
fi
