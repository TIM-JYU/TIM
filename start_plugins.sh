#!/bin/bash

/opt/cs/startPlugins.sh
/opt/svn/startPlugins.sh
/opt/tim/timApp/modules/Haskell/startPlugins.sh
docker network connect timnet csPlugin
docker network connect timnet showFile
docker network connect timnet haskellplugins2
