#!/usr/bin/env bash

set -e

# Double // is intentional; needed for Git Bash.
./run_command_workdir.sh timApp //bin/bash -c "(cd modules/jsrunner/server && npm run buildtools) && npm run b -- $*"
