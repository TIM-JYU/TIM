#!/bin/bash
set -exu
docker build -t timimages/stackage_builder -f Builder.docker $@ .
./build_dumbo.sh
