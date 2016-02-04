docker rm urs
#docker run -privileged -t -i -v /opt/cs:/cs/:ro  -v /tmp/uhome/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114:/home/me/ -w /home/agent cs3 /bin/bash 
# docker run  --name urs --rm=true -t -i -v /opt/cs:/cs/:ro  -v /tmp/uhome/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114:/home/agent/ -w /home/agent cs3 /bin/bash 
# docker run  -t -i -v /var/run/docker.sock:/var/run/docker.sock -v /opt/cs:/cs/:ro  -v /tmp/uhome/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114:/home/agent/ -w /home/agent cs3 /bin/bash 
docker run  --name urss --rm=true  -t -i -v /tmp/uhome/cs:/cst:ro -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):/bin/docker -v /opt/cs:/cs/:ro  -v /tmp/uhome/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114:/home/agent/ -w /home/agent cs3s /bin/bash # /cs/limit.sh  # /cs/rcmd.sh # /bin/bash 
