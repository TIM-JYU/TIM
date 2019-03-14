#!/usr/bin/env bash
# Builds JavaScript of all plugins.
# Currently only needed for csPlugin and showfile.

./run_command_workdir.sh . tsc --build timApp/modules/cs/js timApp/modules/svn/js timApp/modules/pali/js \
timApp/modules/textfield/js timApp/modules/numericfield/js timApp/modules/omaplugin/js timApp/modules/multisave/js
