#!/bin/bash
# Restart csPlugin 

docker stop csPlugin
docker rm csPlugin

# Start csPlugin
pkill csdaemon

sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp

sudo chmod 777 /tmp
sudo mkdir /tmp/uhome
sudo chmod 777 /tmp/uhome 
# sudo mkdir /tmp/uhome/cs
# sudo chmod 777 /tmp/uhome/cs
sudo mkdir /tmp/uhome/run
sudo chmod 777 /tmp/uhome/run
sudo mkdir /tmp/uhome/tmp
sudo chmod 777 /tmp/uhome/tmp
sudo mkdir /tmp/uhome/user
sudo chmod 777 /tmp/uhome/user

sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome
# sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/cs
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/tmp
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/user

# Oikeudet käyttää dockeria niin saadaan docker in docker
sudo chmod 766 /var/run/docker.sock

cd /opt/cs/java
rm comtest*.jar*
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtest.jar
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtestcpp.jar

rm Graphics.jar*
wget https://svn.cc.jyu.fi/srv/svn/ohj1/graphics/trunk/Graphics.jar
rm Ali*.jar
wget https://svn.cc.jyu.fi/srv/svn/ohj2/Ali/trunk/Ali.jar

# Copy Jypeli dll's to temp directory
# sudo cp /opt/cs/jypeli/* /tmp/uhome/cs

cd /opt/cs

# nohup /opt/cs/csdaemon.sh &
# docker run --name csPlugin -d -p 56000:5000 -v /opt/cs:/cs/  -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash -c '/cs/startAll.sh ; /bin/bash'
docker run --name csPlugin -d -p 56000:5000 -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):/bin/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash -c '/cs/startAll.sh ; /bin/bash'
