#!/bin/bash
docker run --rm -v $PWD:/build/ -w /build/ timimages/haskelldev /build/buildScript.sh
