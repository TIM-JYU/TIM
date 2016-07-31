#!/bin/bash 
# Restart pali 

dockername="pali"
dockerOptions="--name $dockername\
           --net=timnet\
           -p 61000:5000\
           -v /opt/tim/timApp/modules/pali:/pali/:ro\
           -v /opt/tim/timApp/modules/py:/py/:ro\
           -v /var/log/pali:/var/log/\
           -w /pali
           pali \
           /bin/bash"

docker stop $dockername
docker rm $dockername

sudo mkdir /var/log/pali
sudo chmod 777 /var/log/pali

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions 
else
    docker run -d $dockerOptions -c './startAll.sh ; /bin/bash' 
fi
 