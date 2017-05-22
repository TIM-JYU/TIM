#!/bin/sh

tce-load -i boot2docker/*.tcz
sudo chmod 766 /var/run/docker.sock
