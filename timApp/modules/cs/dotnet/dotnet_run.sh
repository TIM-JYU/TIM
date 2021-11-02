#!/usr/bin/env bash

DIR="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd)"
dotnet exec --runtimeconfig $DIR/runtimeconfig.json "$@"