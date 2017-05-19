#!/usr/bin/env bash
dockername="cso"
dockerImage="timimages/octave"
 
dockerOptions="--name $dockername --net=timnet -p 56001:5000 -v /var/run/docker.sock:/var/run/docker.sock   -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp2/ -w /cs $dockerImage " 

docker run  --rm=true  -t -i $dockerOptions 