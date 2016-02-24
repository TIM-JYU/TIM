#!/bin/bash
docker run -v $PWD/:/Haskell/ -it haskelldev /bin/bash -c 'cd /Haskell && ./buildSandbox.sh'
