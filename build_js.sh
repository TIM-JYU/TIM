#!/usr/bin/env bash

set -e

./run_command_workdir.sh timApp /bin/bash -c "npm install && npm run lint"
echo "NPM libraries were installed and ESLint was run.

If this is a development instance, you still have to start the 'bdw' NPM script e.g. from your IDE or by running
'npm run bdw' in timApp directory.

If this is not a development instance, run './restart.sh ts' to make sure the ts container picks up the changes.
"
