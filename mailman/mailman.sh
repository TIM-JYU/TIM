#!/bin/bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "$DIR/../docker-compose.sh" exec -e PYTHONPATH=/usr/mailman_scripts mailman-core mailman "$@"