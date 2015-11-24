#!/bin/bash
# Restart csPlugin 
# run with option i to get interactive mode
# option p for pure start (no wget for files)
dockername="csPlugin"
dockerOptions="--name $dockername -p 56000:5000 -v /var/run/docker.sock:/var/run/docker.sock -v $(which docker):/bin/docker -v /opt/cs:/cs/:ro -v /opt/cs/images/cs:/csimages/ -v /tmp/uhome:/tmp/ -w /cs cs3 /bin/bash"

docker stop $dockername
docker rm $dockername

if [ "$2" != "p" ]
then

sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp



sudo chmod 777 /tmp
sudo mkdir -p /tmp/uhome
sudo chmod 777 /tmp/uhome 
# sudo mkdir /tmp/uhome/cs
# sudo chmod 777 /tmp/uhome/cs
sudo mkdir -p /tmp/uhome/run
sudo chmod 777 /tmp/uhome/run
sudo mkdir -p /tmp/uhome/tmp
sudo chmod 777 /tmp/uhome/tmp
sudo mkdir -p /tmp/uhome/user
sudo chmod 777 /tmp/uhome/user
sudo mkdir -p /opt/cs/images/cs
sudo chmod 777 /opt/cs/images/cs

sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome
# sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/cs
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/tmp
sudo setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp/uhome/user

# Oikeudet käyttää dockeria niin saadaan docker in docker
sudo chmod 766 /var/run/docker.sock


# Copy Jypeli dll's  
sudo mkdir -p /opt/cs/jypeli
cd /opt/cs/jypeli
curl http://kurssit.it.jyu.fi/npo/MonoJypeli/TIM/Jypeli.headless.tar.gz | sudo tar -xz --overwrite --warning=none

cd /opt/cs/java
rm -f comtest*.jar*
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtest.jar -O comtest.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtestcpp.jar -O comtestcpp.jar -nv

wget https://svn.cc.jyu.fi/srv/svn/ohj1/graphics/trunk/Graphics.jar -O Graphics.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/ohj2/Ali/trunk/Ali.jar -O Ali.jar -nv


mkdir -p cs
sudo chown -R $(id -un):$(id -gn) cs
cd cs
rm -f comtest*.jar*
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/tojukarp/trunk/dist/ComTest.jar -O ComTest.jar -nv

fi

cd /opt/cs
sudo chmod 777 r

if [ "$1" = "i" ]
then
    # interactive
    docker run  --rm=true  -t -i $dockerOptions 
else
    docker run -d $dockerOptions -c './startAll.sh ; /bin/bash' 
fi
