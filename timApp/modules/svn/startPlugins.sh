#!/bin/bash
# Restart showFile

dockername="showFile"

docker stop $dockername
docker rm $dockername

dockerOptions="--name $dockername -p 55000:5000 -v /opt/svn:/svn/:ro -w /svn svn /bin/bash"

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions 
else
    docker run -d $dockerOptions -c './startAll.sh ; /bin/bash' 
fi
