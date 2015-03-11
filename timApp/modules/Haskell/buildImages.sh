#!/bin/sh
set -euo pipefail
IFS=$'\n\t'

# This build the development environment and the run environment images

docker build -t "haskelldev" .
docker build -t "haskellrun" runEnvironment/

