#!/usr/bin/env bash

# Stop running the script if an error occurs.
set -e 
set -o pipefail

# Install Docker.
apt update
apt install apt-transport-https ca-certificates curl gnupg-agent software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
apt update
apt install -y docker-ce

# Install Docker Compose.
curl -L https://github.com/docker/compose/releases/download/1.28.4/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# Run docker commands without sudo.
usermod -aG docker ubuntu

# Create TIM folder and adjust permissions.
mkdir -p /opt/tim
chown ubuntu:docker /opt/tim
chmod ug+rwxs /opt/tim

# Install TIM.
cd /opt/tim
sudo -u ubuntu git clone https://gitlab.com/tim-jyu/tim.git .
sudo -u ubuntu git submodule update --init
sudo -u ubuntu cp variables.sh.template variables.sh
chmod u+x variables.sh
sed -i 's/^echo variables.sh/#echo variables.sh/' variables.sh
DOMAIN=$(dig +short -x $(dig +short myip.opendns.com @resolver1.opendns.com) | sed 's/.$//')
sed -i "s/localhost/${DOMAIN}/" variables.sh
sed -i "s/http:/https:/" variables.sh
echo Pulling Docker images...
./dc pull --quiet
./npmi
./js
./up.sh

echo Server URL is ${DOMAIN}
echo TIM install script finished.
