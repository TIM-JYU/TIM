#!/usr/bin/env bash

if [ "$SKIP_JSRUNNER_START" = "true" ]; then
    sleep infinity;
fi

npm start
