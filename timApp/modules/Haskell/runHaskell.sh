#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# This runs the haskell development environment

docker run -v /opt/tim/timApp/modules/Haskell/:/Haskell/ -i -t haskelldev /bin/bash
