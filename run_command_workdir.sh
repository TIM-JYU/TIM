#!/usr/bin/env bash
# Runs the given command in the given working directory inside TIM container with the repository's root path being mapped at /service inside container.
# Usage: ./run_command_workdir.sh <working dir> <command> [arguments...]

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Git Bash needs extra "/" in workdir.
. ${DIR}/docker-compose.sh run --rm --workdir="//service/$1" tim "${@:2}"
