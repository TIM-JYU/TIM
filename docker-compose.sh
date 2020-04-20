#!/usr/bin/env bash
# A wrapper script for executing Docker Compose commands.

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

MISSING_FILES=0
if [ ! -f "${DIR}/variables.sh" ]; then
  echo "The file variables.sh does not exist, copying template. See variables.sh for how to proceed."
  cp variables.sh.template variables.sh
  chmod u+x variables.sh
  MISSING_FILES=1
fi

if [ ! -f "${DIR}/docker-compose.prod.yml" ]; then
  echo "The file docker-compose.prod.yml does not exist, copying template.
If there will be multiple TIM instances on this host, delete the 'caddy' section from that file."
  cp docker-compose.prod.yml.template docker-compose.prod.yml
  MISSING_FILES=1
fi

if [ "${MISSING_FILES}" == 1 ]; then
  exit 1
fi

. ${DIR}/variables.sh

if ! [ -z $DEV ]; then
  if [ "$DEV" == 1 ]; then
    IS_DEVELOPMENT=true
  else
    IS_DEVELOPMENT=false
  fi
fi

if [ "$IS_TESTING" = true ]; then
  COMPOSE_PROJECT_NAME="${COMPOSE_PROJECT_NAME}test"
  docker-compose -f "${DIR}/docker-compose.yml" -f "${DIR}/docker-compose.test.yml" "$@"
elif [ "$IS_DEVELOPMENT" = true ]; then
  docker-compose -f "${DIR}/docker-compose.yml" -f "${DIR}/docker-compose.dev.yml" "$@"
else
  docker-compose -f "${DIR}/docker-compose.yml" -f "${DIR}/docker-compose.prod.yml" "$@"
fi
