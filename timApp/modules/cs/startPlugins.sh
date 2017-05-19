#!/usr/bin/env bash
# Start csplugin

if [ "$2" != "p" ]
then

#setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp

if [ ! -d MIRToolbox ]; then
    echo "GET MIR"
    mkdir MIRToolbox
    cd MIRToolbox
    rm -rf *

    #Here we download MIRToolbox:
    git clone http://github.com/martinarielhartmann/mirtooloct .
    #To reduce the size of the folder a bit:
    cd mirtooloct
    rm -rf *.pdf
fi

mkdir -p /tmp/cache
chown -R agent:agent /tmp
rm /tmp/cache/* > /dev/null 2>&1

# Oikeudet käyttää dockeria niin saadaan docker in docker
chmod 766 /var/run/docker.sock

# Copy Jypeli dll's  
cd /cs/jypeli
curl http://kurssit.it.jyu.fi/npo/MonoJypeli/TIM/Jypeli.headless.tar.gz | tar -xz --overwrite --warning=none

cd /cs/java
rm -f comtest*.jar*
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtest.jar -O comtest.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/vesa/trunk/comtestcpp.jar -O comtestcpp.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/ohj1/graphics/trunk/Graphics.jar -O Graphics.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/ohj2/Ali/trunk/Ali.jar -O Ali.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/ohj2/FXExamples/trunk/FXGui/fxgui.jar -O fxgui.jar -nv
wget https://svn.cc.jyu.fi/srv/svn/ohj2/gui/gui.jar -O gui.jar -nv

mkdir -p cs
cd cs
wget https://svn.cc.jyu.fi/srv/svn/comtest/proto/tojukarp/trunk/dist/ComTest.jar -O ComTest.jar -nv

mkdir -p /cs/simcir/check
cd /cs/simcir/check
wget https://yousource.it.jyu.fi/opetus-ji/logik-py/blobs/raw/master/simcirtest.py -O simcirtest.py -nv

fi

cd /cs

./startAll.sh
