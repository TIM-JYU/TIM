#docker run -privileged -t -i -v /opt/cs:/cs/:ro  -v /tmp/uhome/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114:/home/me/ -w /home/agent cs3 /bin/bash 
docker run -privileged -t -i -v /opt/cs:/cs/:ro  -v /tmp/uhome/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114:/home/agent/ -w /home/agent cs3 /bin/bash 
