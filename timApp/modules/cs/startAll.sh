#!/bin/sh
cd /cs
# Set locale
export LANG=en_US.UTF-8
 
# For X server emulation
# Remove any lingering X11 server locks from previous run
rm -f /tmp/.X*-lock
Xvfb $DISPLAY -ac -screen 0 "$XVFB_WHD" -nolisten tcp +extension GLX +render -noreset -nolisten unix &

cd /cs
export CLASSPATH=".:/cs/java/*"

# Run the server.
python3 -O /cs/cs.py
