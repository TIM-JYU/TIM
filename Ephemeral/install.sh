#!/bin/bash
# A script to install everything and build Ephemeral
apt-get install -y ghc cabal-install alex happy zlib1g-dev
cabal update
cabal install --only-dependencies
cabal configure
cabal build
mkdir log

echo "To launch Ephemeral, run"
echo "./dist/build/Ephemeral/Ephemeral -p 8001 &"
