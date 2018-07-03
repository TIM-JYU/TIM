#!/usr/bin/env bash
# This script:
# - Installs the required NPM and JSPM dependencies.
# - Builds all JS bundles (ace, chart, deps, imagex, katex, tim).
# - Creates type declarations for TIM modules (needed for plugins).

# You only need to run this when:
#  - you build TIM for the first time
#  - you add or remove external JS libraries (either from NPM or JSPM)
#  - you modify tim/editor/ace.ts or tim/plugin/imagex.ts (because these are not included in the set of watched files)

# ./run_command_workdir.sh timApp/static/scripts /bin/bash -c "npm install && jspm install && npm run fixLibs && npm run build && tsc"
./run_command_workdir.sh timApp/static/scripts /bin/bash -c "npm install --no-bin-links && jspm install && npm run fixLibs && npm run build && tsc"

