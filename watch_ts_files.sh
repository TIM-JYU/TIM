#!/usr/bin/env bash
# Activates a file watcher that triggers JS rebuild whenever a file belonging to the bundle changes.
# This includes all TypeScript files except tim/ace.ts and tim/imagex.ts.

. ./variables.sh

docker rm -f ts-watcher-${TIM_NAME} 2> /dev/null
DOCKERFLAGS="-d --name ts-watcher-${TIM_NAME}" ./run_command_workdir.sh timApp/static/scripts jspm bundle tim/main + tim/slide - "(tim/main + tim/slide - [tim/**/*])" build/tim.js -wm
echo It takes a while for the watcher to do the initial build, after which it starts watching. Check the status with: docker logs ts-watcher-${TIM_NAME}.
