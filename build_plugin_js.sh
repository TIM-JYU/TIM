#!/usr/bin/env bash

set -e

# When building JSAV, don't use simply "npm install" because it also installs testing-related stuff such as PhantomJS.
# Just install the bare minimum grunt packages required for building it.
./run_command_workdir.sh timApp/modules/cs/jsav bash -c "npm i -D grunt grunt-contrib-concat grunt-contrib-uglify grunt-exec load-grunt-tasks && npx grunt"

./run_command_workdir.sh timApp/modules/jsrunner/server npm install --no-bin-links --unsafe-perm
