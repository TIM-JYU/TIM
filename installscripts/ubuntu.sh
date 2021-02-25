#!/usr/bin/env bash

source ./checkargs.sh

TIM_USER=${USER}

# Install Docker.
apt update
apt install apt-transport-https ca-certificates curl gnupg-agent software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
apt update
apt install -y docker-ce

# Install Docker Compose.
curl -L https://github.com/docker/compose/releases/download/1.28.4/docker-compose-$(uname -s)-$(uname -m) >/usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

source ./common.sh "$@"
