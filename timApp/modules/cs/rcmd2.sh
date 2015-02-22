#!/bin/bash
# Set locale
export LANG=en_US.UTF-8
# For X server emulation
Xvfb :1 -screen 0 1280x1024x24  2>/dev/null  &
export DISPLAY=:1
cmd=$1

if [  -z "$cmd"  ]; then
    cmd="cmd.sh"
fi

# Copy Jypeli dll's to temp directory
# cp /cs/jypeli/* /tmp/

export CLASSPATH=.:/cs/java/junit.jar:/cs/java/hamcrest-core.jar:/cs/java/comtest.jar:/cs/java/Ali.jar:/cs/java/Graphics.jar

#if ! [ -f ".bashrc" ]; then
#    cp /cs/bash/.bashrc ".bashrc"
#fi

# ./.bashrc
chmod 755 ~/.state
~/.state 
# cd /home/me
chmod 755 ~/$cmd
# cp $cmd a.sh
~/$cmd
rm ~/$cmd
export >~/.state
set >>~/.state

 
 
