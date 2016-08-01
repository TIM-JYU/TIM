#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This builds the development environment and the run environment images

docker build -t "timimages/haskelldev" $@ .
docker build -t "timimages/haskellrun" $@ runEnvironment/

./launch_sandbox_build.sh
