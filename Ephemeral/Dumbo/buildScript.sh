#!/bin/bash
set -exu
cd /build
cabal configure
cabal build
chmod -R a+rw dist    
