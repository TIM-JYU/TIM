#!/bin/sh

# Idea from https://gist.github.com/lmakarov/54302df8ecfc87b36320

DOCKER_COMPOSE_VERSION=1.25.3

echo 'Downloading docker-compose to the permanent VM storage...'
sudo mkdir -p /var/lib/boot2docker/bin
sudo curl -sL https://github.com/docker/compose/releases/download/${DOCKER_COMPOSE_VERSION}/docker-compose-`uname -s`-`uname -m` -o /var/lib/boot2docker/bin/docker-compose
sudo chmod +x /var/lib/boot2docker/bin/docker-compose
sudo ln -sf /var/lib/boot2docker/bin/docker-compose /usr/local/bin/docker-compose

echo 'Installing Bash...'
sudo su -c "tce-load -wi bash" docker

echo 'Writing to bootlocal.sh to persist changes...'
cat <<SCRIPT | sudo tee /var/lib/boot2docker/bootlocal.sh > /dev/null
# docker-compose
sudo ln -sf /var/lib/boot2docker/bin/docker-compose /usr/local/bin/docker-compose
sudo su -c "tce-load -wi bash" docker
SCRIPT
sudo chmod +x /var/lib/boot2docker/bootlocal.sh

echo 'Testing docker-compose...'
docker-compose --version

echo 'Testing Bash...'
bash --version
