#!/bin/bash
set -exu
docker build -t stackage_builder -f Builder.docker $@ . 
./build_dumbo.sh
