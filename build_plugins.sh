#!/bin/bash

(cd timApp/modules/Haskell \
 && ./buildImages.sh \
 && docker run -v $PWD/:/Haskell/ -it haskelldev /bin/bash -c 'cd /Haskell && ./buildSandbox.sh')
(cd timApp/modules/cs && ./build.sh)
(cd timApp/modules/svn && ./build.sh)
