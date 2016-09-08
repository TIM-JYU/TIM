#!/bin/bash
# Restart uploader plugin
docker pull villet/uploaderplugin:latest

dockername="uploader"

docker stop $dockername > /dev/null 2>&1
docker rm $dockername > /dev/null 2>&1

dockerOptions="--name $dockername --net=timnet -p 41419:41419 villet/uploaderplugin"

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions
else
    docker run -d $dockerOptions  './StartAll.sh'
fi


