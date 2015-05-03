#!/bin/sh

docker build --tag="tim" timApp
cd Ephemeral
./install.sh
cd -
cd timApp/modules/Haskell
./buildImages.sh
docker run -v $PWD/:/Haskell/ -it haskelldev /bin/bash -c 'cd /Haskell && ./buildSandbox.sh'
cd -
docker build --tag="cs3" timApp/modules/cs
docker build --tag="svn" timApp/modules/svn
