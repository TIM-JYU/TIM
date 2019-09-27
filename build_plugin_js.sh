#!/usr/bin/env bash
# Builds JavaScript of all plugins.
# Currently only needed for csPlugin and showfile.
set -e

./run_command_workdir.sh . tsc --build timApp/modules/cs/js timApp/modules/svn/js timApp/modules/pali/js \
timApp/modules/fields/js   \
timApp/modules/jsrunner/public/javascripts  timApp/modules/feedback/js timApp/modules/drag/js

./run_command_workdir.sh timApp/modules/jsrunner npm install --no-bin-links --unsafe-perm
