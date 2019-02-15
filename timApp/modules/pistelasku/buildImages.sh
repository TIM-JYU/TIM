#!/bin/sh 
set -eu pipefail
IFS=$'\n\t'

# This build the run environment images

docker build --tag "pali" .
