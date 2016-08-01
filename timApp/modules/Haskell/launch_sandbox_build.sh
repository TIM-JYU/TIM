#!/bin/bash
docker run -v $PWD/:/Haskell/ -it timimages/haskelldev /bin/bash -c 'cd /Haskell && ./buildSandbox.sh'
