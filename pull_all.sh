#!/bin/bash

# Pulls the latest TIM images from the timimages repository.

docker pull ubuntu

docker pull timimages/tim:$(./get_latest_date.sh)
docker pull timimages/cs3
docker pull timimages/svn
docker pull timimages/haskellrun
docker pull timimages/haskelldev
docker pull timimages/stackage_builder
docker pull timimages/postgre
docker pull timimages/pali
docker pull timimages/local_nginx
docker pull timimages/funnel

docker tag timimages/cs3 cs3
docker tag timimages/svn svn
docker tag timimages/stackage_builder stackage_builder
docker tag timimages/postgre postgre
docker tag timimages/pali pali
docker tag timimages/funnel funnel

# Remove leftover untagged images
docker rmi $(docker images | grep "^<none>" | awk '{print $3}')

# Haskell plugin binaries are not included directly in the images, so we need to build them
(cd timApp/modules/Haskell && ./launch_sandbox_build.sh $@)

# Same with Dumbo
(cd Ephemeral/Dumbo && ./build_dumbo.sh $@)

./create_network.sh
