#!/bin/bash
docker run --rm -v $PWD:/build/ -w /build/ timimages/stackage_builder
