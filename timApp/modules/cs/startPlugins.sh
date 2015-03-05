#!/bin/bash
# Restart csPlugin 

docker stop csPlugin
docker rm csPlugin

# Start csPlugin
pkill csdaemon

sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp

chmod 777 /tmp
mkdir /tmp/uhome
chmod 777 /tmp/uhome
mkdir /tmp/uhome/cs
chmod 775 /tmp/uhome/cs
mkdir /tmp/uhome/run
chmod 777 /tmp/uhome/run

sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/cs
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/tmp

cd /opt/cs/java
rm comtest*.jar*
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtest.jar
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtestcpp.jar

rm Graphics.jar*
wget https://svn.cc.jyu.fi/srv/svn/ohj1/graphics/trunk/Graphics.jar
rm Ali*.jar
wget https://svn.cc.jyu.fi/srv/svn/ohj2/Ali/trunk/Ali.jar

# Copy Jypeli dll's to temp directory
sudo cp /opt/cs/jypeli/* /tmp/uhome/cs

cd /opt/cs

nohup /opt/cs/csdaemon.sh &
# docker run --name csPlugin -d -p 56000:5000 -v /opt/cs:/cs/  -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash -c '/cs/startAll.sh ; /bin/bash'
docker run --name csPlugin -d -p 56000:5000 -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):/bin/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash -c '/cs/startAll.sh ; /bin/bash'
