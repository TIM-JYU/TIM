#!/usr/bin/env bash
# Builds the NPM package at timApp/static/scripts.

./run_command_workdir.sh timApp/static/scripts npm install
./run_command_workdir.sh timApp/static/scripts jspm install
./run_command_workdir.sh timApp/static/scripts npm run build
