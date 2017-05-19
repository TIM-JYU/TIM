#!/usr/bin/env bash
# Set locale
export LANG=en_US.UTF-8
# export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/

if [ $2 != "True" ]; then
  # For X server emulation
  Xvfb :1 -screen 0 1280x1024x24 -extension RANDR 2>/dev/null  &
  export DISPLAY=:1 
fi
export GNUTERM=png
cmd=$1

eval savestate=$3

if [  -z "$cmd"  ]; then
    cmd="cmd.sh"
fi
 
# Copy Jypeli dll's to temp directory
# cp /cs/jypeli/* /tmp/

export CLASSPATH=.:/cs/java/junit.jar:/cs/java/hamcrest-core.jar:/cs/java/comtest.jar:/cs/java/Ali.jar:/cs/java/Graphics.jar:/cs/java/fxgui.jar:/cs/java/gui.jar
export MONO_PATH=/cs/jypeli

#if ! [ -f ".bashrc" ]; then
#    cp /cs/bash/.bashrc ".bashrc"
#fi

# ./.bashrc
if  ! [  -z "$savestate"  ] && [ -f $savestate ]; then
    chmod 755 $savestate
    source $savestate
fi
# cd /home/agent/run
# echo "Running: $cmd" >> /tmp/log/log.txt
# echo "Running: $cmd" >> log.txt
#set +e
#chmod 755 ~/$cmd || echo ""
#set -e
#cp $cmd a.sh

if ! [  -z "$savestate"  ]; then
    cd $PWD # ei tässä kun kaikki eivät kestä muutosta, mutta ilman tätä ei aloita edellisestä hakemistosta
fi

ulimit -f 200000 # -t 1 -v 2000 -s 100 -u 10

source ~/$cmd

if ! [  -z "$savestate"  ]; then
    pwd >~/pwd.txt
    export >$savestate
fi
rm ~/$cmd # Tämä ansiosta csRun jatkaa sitten suorittamista ja lukee inputin
#set >>~/.state

 
 
