#!/usr/bin/env bash
# Runs the given command in the given working directory inside TIM container with the repository's root path being mapped at /service inside container.
# Usage: ./run_command_workdir.sh [--dc-no-deps] <working dir> <command> [arguments...]

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# parse all args using getopts and check for --dc-no-deps
docker_args=""
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        --dc-no-deps)
            docker_args="--no-deps"
            shift
            ;;
        *)
            break
            ;;
    esac
done

# Git Bash needs extra "/" in workdir.
. ${DIR}/docker-compose.sh run --rm $docker_args --workdir="//service/$1" tim "${@:2}"
