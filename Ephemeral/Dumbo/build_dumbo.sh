#!/bin/bash
docker run --rm -v $PWD:/build/ -w /build/ stackage_builder
