#!/usr/bin/env bash
# Set locale
export LANG=en_US.UTF-8
# export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/
export CLASSPATH=.:/cs/java/junit.jar:/cs/java/hamcrest-core.jar:/cs_data/java/comtest.jar:/cs_data/java/Ali.jar:/cs_data/java/Graphics.jar:/cs_data/java/fxgui.jar:/cs_data/java/gui.jar
export PATH="$PATH:/cs/dotnet"

# Create symlinks for some data folders as they are expected to be found in /cs
ln -s /cs_data/MIRToolbox /cs/MIRToolbox
ln -s /cs_data/simcir /cs/simcir

printf "\n" >~/run/time.txt
if [ -e run/compile.sh ]
    then
    printf "Compile time:" >>~/run/time.txt
    (time run/compile.sh) &>> ~/run/time.txt
    if [ $? -ne 0 ]
    then
       (>&2 echo "Compile error")
       exit
    fi
fi

if [ $2 != "True" ]; then
  # For X server emulation
  Xvfb $DISPLAY -ac -screen 0 "$XVFB_WHD" -nolisten tcp +extension GLX +render -noreset -nolisten unix 1>&2 &
fi
export GNUTERM=png
cmd=$1

eval savestate=$3

if [  -z "$cmd"  ]; then
    cmd="cmd.sh"
fi
#
if  ! [  -z "$savestate"  ] && [ -f $savestate ]; then
    chmod 755 $savestate
    source $savestate
fi

if ! [  -z "$savestate"  ]; then
    cd $PWD # ei tässä kun kaikki eivät kestä muutosta, mutta ilman tätä ei aloita edellisestä hakemistosta
fi

ulimit -f 200000 # -t 1 -v 2000 -s 100 -u 10

printf '\nRun time:' >>~/run/time.txt
(time source ~/$cmd

if ! [  -z "$savestate"  ]; then
    pwd >~/pwd.txt
    export >$savestate
fi
) &>> ~/run/time.txt
rm ~/$cmd # Tämä ansiosta csRun jatkaa sitten suorittamista ja lukee inputin

