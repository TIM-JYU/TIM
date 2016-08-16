#!/bin/bash 
# Restart pali 

dockerImage="pali"
extraPorts=""
extraCmds="/bin/bash"

if [ "$2" = "d" ]
then
    dockerImage="palisudo"
    extraPorts="-p 49997:22"
    extraCmds=''
fi



dockername="pali"
dockerOptions="--name $dockername\
           --net=timnet\
           -p 61000:5000 $extraPorts\
           -v /opt/tim/timApp/modules/pali:/pali/:ro\
           -v /opt/tim/timApp/modules/py:/py/:ro\
           -v /var/log/pali:/var/log/\
           -w /pali
           $dockerImage \
           $extraCmds"

docker stop $dockername
docker rm $dockername

sudo mkdir /var/log/pali
sudo chmod 777 /var/log/pali

echo $dockerOptions

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions 
else
    docker run -d $dockerOptions -c './startAll.sh ; /bin/bash' 
fi
 