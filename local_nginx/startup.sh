#!/bin/bash
/bin/sed -i "s/DOCKER_BRIDGE_IP/${DOCKER_BRIDGE}/" /etc/nginx/sites-enabled/tim.conf
/usr/sbin/nginx
