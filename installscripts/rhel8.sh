#!/usr/bin/env bash

source ./checkargs.sh

TIM_USER=${USER}

# Install Docker.
yum -y module remove container-tools
yum-config-manager -y --add-repo https://download.docker.com/linux/centos/docker-ce.repo
yum -y install docker-ce git
systemctl start docker
systemctl enable docker.service
systemctl enable containerd.service

# Install Docker Compose.
curl -L https://github.com/docker/compose/releases/download/1.28.4/docker-compose-$(uname -s)-$(uname -m) >/bin/docker-compose
chmod +x /bin/docker-compose

source ./common.sh "$@"
