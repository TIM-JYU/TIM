#!/bin/sh
set -euo pipefail
IFS=$'\n\t'

# This runs the haskell development environment

docker run -v $PWD/:/Haskell/ -i -t timimages/haskelldev /bin/bash
