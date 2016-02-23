#!/bin/bash

# Pulls the latest TIM images from the timimages repository.

docker pull ubuntu

docker pull -a timimages/tim
docker pull timimages/cs3
docker pull timimages/svn
docker pull timimages/haskellrun
docker pull timimages/haskelldev
docker pull timimages/stackage_builder
docker pull timimages/postgre
docker pull timimages/pali

docker tag timimages/tim:$(./get_latest_date.sh) tim:$(./get_latest_date.sh)
docker tag timimages/cs3 cs3
docker tag timimages/svn svn
docker tag timimages/haskellrun haskellrun
docker tag timimages/haskelldev haskelldev
docker tag timimages/stackage_builder stackage_builder
docker tag timimages/postgre postgre
docker tag timimages/pali pali

# Remove leftover untagged images
docker rmi $(docker images | grep "^<none>" | awk '{print $3}')
