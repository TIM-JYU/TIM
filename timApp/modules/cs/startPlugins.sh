#!/bin/bash
# Restart csPlugin 
# run with option i to get interactive mode
docker stop csPlugin
docker rm csPlugin


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
sudo mkdir /opt/cs/images
sudo mkdir /opt/cs/images/cs
sudo chmod 777 /opt/cs/images/cs

sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome
# sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/cs
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/tmp
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/user

# Oikeudet käyttää dockeria niin saadaan docker in docker
sudo chmod 766 /var/run/docker.sock

sudo chmod 777 r

mkdir /opt/cs/jypeli
cd /opt/cs/jypeli
curl http://kurssit.it.jyu.fi/npo/MonoJypeli/TIM/Jypeli.headless.tar.gz | sudo tar -xz --overwrite

cd /opt/cs/java
rm -f comtest*.jar*
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtest.jar -O comtest.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtestcpp.jar -O comtestcpp.jar -nv

rm -f Graphics.jar*
wget https://svn.cc.jyu.fi/srv/svn/ohj1/graphics/trunk/Graphics.jar -O Graphics.jar - nv
wget https://svn.cc.jyu.fi/srv/svn/ohj2/Ali/trunk/Ali.jar -O Ali.jar -nv


sudo mkdir cs
sudo chmod 777 cs
cd cs
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/tojukarp/trunk/dist/ComTest.jar -O ComTest.jar -nv


# Copy Jypeli dll's to temp directory
# sudo cp /opt/cs/jypeli/* /tmp/uhome/cs

cd /opt/cs

# nohup /opt/cs/csdaemon.sh &
# docker run --name csPlugin -d -p 56000:5000 -v /opt/cs:/cs/  -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash -c '/cs/startAll.sh ; /bin/bash'
if [ "$1" = "i" ]
then
    # interactive
    docker run  --name csPlugin --rm=true  -t -i -p 56000:5000 -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):/bin/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash 
else
    docker run --name csPlugin -d -p 56000:5000 -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):/bin/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash -c '/cs/startAll.sh ; /bin/bash'
fi
