#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cd /Haskell
if [ ! -d ".cabal-sandbox/" ]; 
 then
  cabal sandbox init
  cabal update;
fi
cabal install --only-dependencies PluginConstructionKit/ Choices/
