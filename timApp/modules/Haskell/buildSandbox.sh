#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This is used by the container to rebuild the cabal sandbox if necessary

cd /Haskell
if [ ! -d ".cabal-sandbox/" ]; 
 then
  cabal sandbox init
  cabal update;
fi

gitGet () {
    if [ ! -d "$1" ]; 
     then
         git clone $2 $1
     else
         if [ ! -d "$1/.git" ];
          then
            mv $1 orig-$1
            git clone $2 $1
            cp -a orig-$1/. $1
            rm -rf orig-$1
          else
            (cd $1 && git stash && git pull && (git stash pop || true))
         fi
    fi
}
gitGet PluginConstructionKit git://yousource.it.jyu.fi/ties343-funktio-ohjelmointi/PluginConstructionKit.git
gitGet Choices git://yousource.it.jyu.fi/ties343-funktio-ohjelmointi/MultipleChoicePlugin.git

cabal install PluginConstructionKit/ Choices/
