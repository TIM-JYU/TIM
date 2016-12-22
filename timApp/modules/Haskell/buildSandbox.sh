#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cd /Haskell

(cd Choices/ \
&& stack solver --update-config --allow-different-user \
&& stack build --allow-different-user --copy-bins)
