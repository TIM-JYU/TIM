#!/usr/bin/env bash
# This script:
# - Installs the required NPM and JSPM dependencies.
# - Builds all JS bundles (ace, chart, deps, imagex, katex, tim).
# - Creates type declarations for TIM modules (needed for plugins).

# You only need to run this when:
#  - you build TIM for the first time
#  - you add or remove external JS libraries (either from NPM or JSPM)
#  - you modify tim/ace.ts or tim/imagex.ts (because these are not included in the set of watched files)

./run_command_workdir.sh timApp/static/scripts npm install
./run_command_workdir.sh timApp/static/scripts jspm install
./run_command_workdir.sh timApp/static/scripts npm run fixAll
./run_command_workdir.sh timApp/static/scripts npm run build

# Create type declarations.
./run_command_workdir.sh timApp/static/scripts tsc
