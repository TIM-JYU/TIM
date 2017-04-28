#!/usr/bin/env bash

. ./variables.sh

docker rm -f ts-watcher-${TIM_NAME} 2> /dev/null

# Launches a file watcher for TypeScript files and rebuilds JS on the fly.
DOCKERFLAGS="-d --name ts-watcher-${TIM_NAME}" ./run_command_workdir.sh timApp/static/scripts jspm bundle tim/main + tim/slide - "(tim/main + tim/slide - [tim/**/*])" build/tim.js -wm
