#!/usr/bin/env bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
git --git-dir=${DIR}/.git --work-tree=${DIR} log -1 --date=short --format="%cd-%h" $DIR/timApp/Dockerfile
