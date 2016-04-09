#!/bin/bash
set -exu
cd /build
stack build --allow-different-user
mkdir -p dist/build/Dumbo
cp /build/.stack-work/install/x86_64-linux/lts-3.1/7.10.2/bin/Dumbo dist/build/Dumbo/
