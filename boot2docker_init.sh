#!/bin/sh

tce-load -i boot2docker/*.tcz
sudo chmod 766 /var/run/docker.sock
sudo rmdir /tmp/uhome 2> /dev/null

# Disable wget's certificate check; otherwise some wget calls in csPlugin's start script will fail
echo check_certificate = off > $HOME/.wgetrc
