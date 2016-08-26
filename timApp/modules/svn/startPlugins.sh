#!/bin/bash
# Restart showFile

dockername="showfile"

docker stop $dockername > /dev/null 2>&1
docker rm $dockername > /dev/null 2>&1

dockerOptions="--name $dockername --net=timnet -p 55000:5000 -v /tmp:/tmps/ -v /tmp/uhome:/tmp/ -v /opt/svn:/svn/:ro -w /svn svn /bin/bash"

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions 
else
    docker run -d $dockerOptions -c './startAll.sh ; /bin/bash' 
fi
