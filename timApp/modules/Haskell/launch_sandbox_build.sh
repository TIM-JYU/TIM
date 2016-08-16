#!/bin/bash
docker run --rm -v $PWD/:/Haskell/ -v $PWD/bin/:/root/.local/bin/ -it timimages/haskelldev /bin/bash -c 'cd /Haskell && ./buildSandbox.sh'
