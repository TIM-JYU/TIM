#!/bin/bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
git log -1 --date=short --format="%cd-%h" $DIR/timApp/Dockerfile
