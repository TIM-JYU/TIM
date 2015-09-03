#!/bin/bash
set -exu
docker build -t stackage_builder -f Builder.docker . 
docker run --rm -v $PWD:/build/ -w /build/ stackage_builder
docker build -t dumbo_deploy -f Running.docker . 
