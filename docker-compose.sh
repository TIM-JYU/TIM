#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ ! -f ./variables.sh ]; then
  echo "The file variables.sh does not exist, copying template. See variables.sh for how to proceed."
  cp variables.sh.template variables.sh
  chmod u+x variables.sh
  exit 1
fi

. ${DIR}/variables.sh

if [ "$IS_DEVELOPMENT" = true ]; then
  docker-compose -f "${DIR}/docker-compose.yml" -f "${DIR}/docker-compose.dev.yml" "$@"
else
  docker-compose "$@"
fi
