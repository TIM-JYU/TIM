#!/bin/bash
set -exu
cd /build
stack solver --system-ghc --update-config --allow-different-user
stack build --copy-bins --system-ghc --allow-different-user
