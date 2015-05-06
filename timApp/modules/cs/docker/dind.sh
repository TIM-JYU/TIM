docker rm dind   
# docker run  --name dind --rm=true --privileged -t -i -v /var/lib/docker:/var/lib/docker -v /opt/cs:/opt/cs/:ro  -v /tmp/:/tmp/ -w /opt/cs dind wrapdocker;/bin/bash 
docker run  --name dind --rm=true --privileged -t -i -v /var/lib/docker:/var/lib/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs dind wrapdocker;/bin/bash 
# docker run  --name dind --privileged -t -i -v /var/lib/docker:/var/lib/docker   dind /bin/bash 
docker rm dind   
