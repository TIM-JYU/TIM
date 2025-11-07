#!/usr/bin/env bash
# Start csplugin

if [ "$2" != "p" ]
then

DATA_PATH="/cs_data"

#setfacl  -R -d -m m::rwx -m group::rwx -m other::rwx /tmp

MIR_PATH="$DATA_PATH/MIRToolbox"

if [ ! -d "$MIR_PATH" ]; then
    echo "GET MIR"
    mkdir "$MIR_PATH"
    cd "$MIR_PATH"
    rm -rf *

    #Here we download MIRToolbox:
    git clone https://github.com/martinarielhartmann/mirtooloct .
fi

GLOWSCRIPT_PATH="$DATA_PATH/static/glowscript"

if [ ! -d "$GLOWSCRIPT_PATH" ]; then
    echo "GET GlowScript"
    mkdir -p "$GLOWSCRIPT_PATH"
    git clone https://github.com/vpython/glowscript "$GLOWSCRIPT_PATH"
fi

JSAV_PATH="$DATA_PATH/static/jsav"

if [ ! -d "$JSAV_PATH" ]; then
    echo "GET JSAV"
    mkdir -p "$JSAV_PATH"

    wget https://github.com/TIM-JYU/JSAV/releases/download/latest/JSAV.zip -O "$JSAV_PATH/JSAV.zip"
    unzip "$JSAV_PATH/JSAV.zip" -d "$JSAV_PATH"
    rm "$JSAV_PATH/JSAV.zip"
fi

mkdir -p /tmp/cache
chown -R agent:agent /tmp
rm /tmp/cache/* > /dev/null 2>&1

chown -R agent:agent /csgenerated
chown -R agent:agent /logs
#touch /cs/log.txt
#chown agent:agent /cs/log.txt

# TODO: This might not be necessary because we have to run as root anyway - in Docker Desktop for Windows,
#  it's not possible to change socket permissions (it has no effect).
chmod 766 /var/run/docker.sock

if [ "$CSPLUGIN_TARGET" != "base" ]; then
  # Refresh dotnet dependency cache
  cd /cs/dotnet
  ./gen_deps.py /cs_data/dotnet
fi

mkdir -p "$DATA_PATH/java/cs"
cd "$DATA_PATH/java"
wget https://gitlab.jyu.fi/tie/ohj2/esimerkit/comtest/-/raw/main/comtest.jar -O comtest.jar.tmp -nv && mv comtest.jar.tmp comtest.jar
wget https://gitlab.jyu.fi/tie/ohj2/esimerkit/comtest/-/raw/main/comtestcpp.jar -O comtestcpp.jar.tmp -nv && mv comtestcpp.jar.tmp comtestcpp.jar
wget https://gitlab.jyu.fi/tie/ohj1/graphics/-/raw/main/Graphics.jar -O Graphics.jar.tmp -nv && mv Graphics.jar.tmp Graphics.jar
wget https://gitlab.jyu.fi/tie/ohj2/esimerkit/ali/-/raw/main/Ali.jar -O Ali.jar.tmp -nv && mv Ali.jar.tmp Ali.jar
wget https://gitlab.jyu.fi/tie/ohj2/esimerkit/fxexamples/-/raw/master/FXGui/fxgui.jar --no-check-certificate -O fxgui.jar.tmp -nv && mv fxgui.jar.tmp fxgui.jar
wget https://gitlab.jyu.fi/tie/ohj2/esimerkit/gui/-/raw/main/gui.jar -O gui.jar.tmp -nv && mv gui.jar.tmp gui.jar

cd cs
wget https://kurssit.it.jyu.fi/npo/ComTest/ComTest.jar -O ComTest.jar.tmp -nv && mv ComTest.jar.tmp ComTest.jar

cd /cs
rm -rf "$DATA_PATH/simcir"
mkdir -p "$DATA_PATH/simcir/check"
cp -r simcir "$DATA_PATH/simcir"
cd "$DATA_PATH/simcir/check"
wget https://gitlab.jyu.fi/arjuvi/logik-py/-/raw/master/simcirtest.py -O simcirtest.py.tmp -nv && mv simcirtest.py.tmp simcirtest.py

fi

cd /cs

if [ ! -f /root/.ssh/id_rsa ]; then
    echo "Creating ssh keys"
    mkdir /root/.ssh
    ssh-keygen -t rsa -N '' -f /root/.ssh/id_rsa -C csPlugin@TIM
    chmod 700 /root/.ssh
    chmod 600 /root/.ssh/id_rsa
    chmod 644 /root/.ssh/id_rsa.pub
    echo "Public key:"
    cat /root/.ssh/id_rsa.pub
fi

./startAll.sh
