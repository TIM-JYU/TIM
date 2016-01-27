#!/bin/bash

(cd timApp/modules/Haskell && ./buildImages.sh $@)
(cd timApp/modules/cs      && ./buildImages.sh $@)
(cd timApp/modules/svn     && ./buildImages.sh $@)
