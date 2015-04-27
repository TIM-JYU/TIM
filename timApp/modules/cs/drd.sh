#!/bin/sh

docker stop csPlugin
docker rm csPlugin

cd /opt/cs/java
rm comtest*.jar*
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtest.jar
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtestcpp.jar

rm Graphics.jar*
wget https://svn.cc.jyu.fi/srv/svn/ohj1/graphics/trunk/Graphics.jar
rm Ali*.jar
wget https://svn.cc.jyu.fi/srv/svn/ohj2/Ali/trunk/Ali.jar

# Copy Jypeli dll's to temp directory
mkdir /tmp/uhome
mkdir /tmp/uhome/cs
sudo cp /opt/cs/jypeli/* /tmp/uhome/cs


# docker run --name csPlugin -t -i -p 56000:5000 -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash  
# docker run  --name csPluginD --rm=true --privileged -t -i -p 56001:5000 -v /var/lib/docker:/var/lib/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs dind wrapdocker;/bin/bash 
docker run  --name csPlugin --rm=true  -t -i -p 56000:5000 -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):/bin/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash 

