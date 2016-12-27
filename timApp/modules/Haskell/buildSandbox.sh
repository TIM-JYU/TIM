#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cd /Haskell

(cd Choices/ \
&& stack solver --system-ghc --update-config --allow-different-user \
&& stack build --system-ghc --allow-different-user --copy-bins)
