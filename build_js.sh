#!/usr/bin/env bash

set -e

./run_command_workdir.sh timApp /bin/bash -c "npm install --unsafe-perm && npm run lint && npm run b"
