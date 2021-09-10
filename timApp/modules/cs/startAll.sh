#!/bin/sh
cd /cs
# Set locale
export LANG=en_US.UTF-8
 
# For X server emulation
# Remove any lingering X11 server locks from previous run
rm -f /tmp/.X*-lock
Xvfb $DISPLAY -screen 0 "$XVFB_WHD" -nolisten tcp -nolisten unix &

cd /cs
export CLASSPATH=".:/cs/java/*"
export MONO_PATH=/cs/jypeli

# Run the server.
python3 -O /cs/cs.py
