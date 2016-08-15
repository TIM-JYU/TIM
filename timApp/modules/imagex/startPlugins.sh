#!/bin/bash 
# Restart imagex 

dockername="imagex"
dockerOptions="--name $dockername\
           --net=timnet\
           -p 62000:5000\
           -v /opt/tim/timApp/modules/imagex:/imagex/:ro\
           -v /opt/tim/timApp/modules/py:/py/:ro\
           -v /var/log/imagex:/var/log/\
           -w /imagex
           svn \
           /bin/bash"

docker stop $dockername
docker rm $dockername

sudo mkdir /var/log/imagex
sudo chmod 777 /var/log/imagex

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions 
else
    docker run -d $dockerOptions -c './startAll.sh ; /bin/bash' 
fi
 
