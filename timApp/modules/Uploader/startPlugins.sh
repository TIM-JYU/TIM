#!/usr/bin/env bash
# Restart uploader plugin
dockerimage=villet/uploaderplugin:13_10_16_15
docker pull $dockerimage

dockername="uploader"

docker stop $dockername > /dev/null 2>&1
docker rm $dockername > /dev/null 2>&1

dockerOptions="--name $dockername --net=timnet -p 41419:41419 $dockerimage"

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions
else
    docker run -d $dockerOptions  './StartAll.sh'
fi
