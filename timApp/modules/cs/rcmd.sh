#!/bin/bash
# Set locale
export LANG=en_US.UTF-8
# export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/
echo $2 > t.t

if [ $2 != "True" ]; then
  # For X server emulation
  Xvfb :1 -screen 0 1280x1024x24 -extension RANDR 2>/dev/null  &
  export DISPLAY=:1 
  echo "USE X11" >> t.t
fi
export GNUTERM=png
cmd=$1

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
if [ -f ~/.state ]; then
    chmod 755 ~/.state
    source ~/.state 
fi    
# cd /home/agent/run
# echo "Running: $cmd" >> /tmp/log/log.txt
# echo "Running: $cmd" >> log.txt
#chmod 755 ~/$cmd
#cp $cmd a.sh
#cd $PWD # ei tässä kun kaikki eivät kestä muutosta
ulimit -f 80000 # -t 1 -v 2000 -s 100 -u 10
source ~/$cmd
pwd >~/pwd.txt
rm ~/$cmd # Tämä ansiosta csRun jatkaa sitten suorittamista ja lukee inputin
export >~/.state
#set >>~/.state

 
 
