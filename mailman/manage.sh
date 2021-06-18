#!/bin/bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "$DIR/../docker-compose.sh" exec mailman-web python3 manage.py "$@"