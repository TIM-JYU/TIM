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

if [ "${MISSING_FILES}" == 1 ]; then
  exit 1
fi

. ${DIR}/variables.sh

if ! [ -z $DEV ]; then
  if [ "$DEV" == 1 ]; then
    COMPOSE_PROFILES=dev
  else
    COMPOSE_PROFILES=prod
  fi
fi

if [[ "$COMPOSE_PROFILES" == *"dev_mailman"* ]]; then
  export CADDY_EXTRA_TIM_CONFIG="import mailman_dev"
  export CADDY_MAILMAN_STATIC_DIR="./mailman/web/static"
fi

if [[ "$COMPOSE_PROFILES" == *"test"* ]]; then
  COMPOSE_PROJECT_NAME="${COMPOSE_PROJECT_NAME}test"
  docker-compose -f "${DIR}/docker-compose.yml" --profile test "$@"
elif [[ "$COMPOSE_PROFILES" == *"dev_mailman"* ]]; then
  docker-compose -f "${DIR}/docker-compose.yml" -f "${DIR}/docker-compose.dev.yml" -f "${DIR}/mailman/docker-compose.dev.yml" "$@"
elif [[ "$COMPOSE_PROFILES" == *"dev"* ]]; then
  docker-compose -f "${DIR}/docker-compose.yml" -f "${DIR}/docker-compose.dev.yml" "$@"
else
  docker-compose -f "${DIR}/docker-compose.yml" "$@"
fi
