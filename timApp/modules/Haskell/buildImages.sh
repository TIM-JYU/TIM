#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This builds the development environment and the run environment images

docker build -t "haskelldev" $@ .
docker build -t "haskellrun" $@ runEnvironment/

./launch_sandbox_build.sh
