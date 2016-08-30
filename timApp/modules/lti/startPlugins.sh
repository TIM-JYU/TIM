#!/bin/bash 
# Restart LTI

dockername="lti"
dockerOptions="--name $dockername\
           --net=timnet\
	   --cpuset-cpus="0" \
           -p 63000:5000\
           -v /opt/tim/timApp/modules/lti:/lti/:ro\
           -v /opt/tim/timApp/modules/py:/py/:ro\
           -v /var/log/lti:/var/log/\
           -w /lti
           lti \
           /bin/bash"

docker stop $dockername
docker rm $dockername

sudo mkdir /var/log/lti
sudo chmod 777 /var/log/lti

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions 
else
    docker run -t -i -d $dockerOptions -c './startAll.sh ; /bin/bash'
fi
 
