#!/bin/sh

tce-load -wi acl.tcz
tce-load -wi bash.tcz
tce-load -wi wget.tcz
sudo chmod 766 /var/run/docker.sock
sudo rmdir /tmp/uhome

# Disable wget's certificate check; otherwise some wget calls in csPlugin's start script will fail
echo check_certificate = off > $HOME/.wgetrc
