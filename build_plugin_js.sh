#!/usr/bin/env bash
# Builds JavaScript of all plugins.
# Currently only needed for csPlugin and showfile.
set -e

./run_command_workdir.sh timApp/modules/cs/jsav bash -c "npm install && grunt"

./run_command_workdir.sh timApp/modules/jsrunner npm install --no-bin-links --unsafe-perm
