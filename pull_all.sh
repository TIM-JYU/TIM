#!/bin/bash

# Pulls the latest TIM images from the timimages repository and downloads prebuilt binaries.

docker pull ubuntu

docker pull timimages/tim:$(./get_latest_date.sh)
docker pull timimages/cs3
docker pull timimages/svn
docker pull timimages/haskellrun
docker pull timimages/pali
docker pull timimages/local_nginx
docker pull timimages/funnel

# Remove leftover untagged images
docker rmi $(docker images | grep "^<none>" | awk '{print $3}')

# download prebuilt Dumbo
mkdir -p Ephemeral/Dumbo/dist/build/Dumbo
docker run --rm -v ${PWD}/Ephemeral/Dumbo/dist/build/Dumbo:/target timimages/prebuilt /bin/bash -c "cp /bin/Dumbo /target/"

# download prebuilt Haskell plugins (ChoicesPlugin, MultipleChoicesPlugin, ShortNotePlugin, GraphVizPlugin)
mkdir -p timApp/modules/Haskell/bin
docker run --rm -v ${PWD}/timApp/modules/Haskell/bin:/target timimages/prebuilt /bin/bash -c "cp /bin/*Plugin /target/"

(cd timApp/modules/Haskell \
 && git clone git://yousource.it.jyu.fi/ties343-funktio-ohjelmointi/MultipleChoicePlugin.git Choices 2> /dev/null \
 ; cd Choices && git pull)

./create_network.sh
