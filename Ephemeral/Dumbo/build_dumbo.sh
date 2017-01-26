#!/bin/bash
docker run --rm -v $PWD/dist/build/Dumbo/:/root/.local/bin/ -v $PWD:/build/ -w /build/ timimages/haskelldev /build/buildScript.sh
