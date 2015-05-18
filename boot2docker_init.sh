#!/bin/sh

tce-load -wi acl.tcz
tce-load -wi bash.tcz
tce-load -wi wget.tcz
sudo chmod 766 /var/run/docker.sock
sudo rmdir /tmp/uhome
