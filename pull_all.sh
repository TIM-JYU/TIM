#!/bin/bash

# Pulls the latest TIM images from the timimages repository and downloads prebuilt binaries.

. ./variables.sh

# TIM and Haskell plugins must not be running
docker stop ${TIM_NAME} 2> /dev/null
docker stop haskellplugins2 2> /dev/null

docker pull ubuntu

docker pull timimages/tim:$(./get_latest_date.sh)
docker pull timimages/cs3
docker pull timimages/svn
docker pull timimages/haskellrun
docker pull timimages/pali
docker pull timimages/local_nginx
docker pull timimages/funnel
docker pull timimages/prebuilt

# Remove leftover untagged images
docker rmi $(docker images -q --filter dangling=true) 2> /dev/null

# download prebuilt Dumbo
mkdir -p Ephemeral/Dumbo/dist/build/Dumbo
docker run --rm -v ${PWD}/Ephemeral/Dumbo/dist/build/Dumbo:/target timimages/prebuilt /bin/bash -c "cp /bin/Dumbo /target/"

# download prebuilt Haskell plugins (ChoicesPlugin, MultipleChoicesPlugin, ShortNotePlugin, GraphVizPlugin)
mkdir -p timApp/modules/Haskell/bin
docker run --rm -v ${PWD}/timApp/modules/Haskell/bin:/target timimages/prebuilt /bin/bash -c "cp /bin/*Plugin /target/"

git submodule update --recursive --remote

./create_network.sh 2> /dev/null
