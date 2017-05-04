#!/usr/bin/env bash
# Builds the NPM package at timApp/static/scripts.
# You only need to run this when:
#  - you build TIM for the first time
#  - you add or remove external JS libraries (either from NPM or JSPM)
#  - you modify tim/ace.ts or tim/imagex.ts (because these are not included in the set of watched files)

./run_command_workdir.sh timApp/static/scripts npm install
./run_command_workdir.sh timApp/static/scripts jspm install
./run_command_workdir.sh timApp/static/scripts npm run fixAll
./run_command_workdir.sh timApp/static/scripts npm run build

# This is only needed to get the type declarations for plugins.
./run_command_workdir.sh timApp/static/scripts tsc

./watch_ts_files.sh
