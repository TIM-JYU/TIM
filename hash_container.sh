#!/usr/bin/env bash
# Computes sha1 hash of all the files passed as arguments.

for file in "$@"; do sha1="${sha1}$(< "${file}" tr -d '\r\n')"; done
echo "${sha1}" | sha1sum | tr -d ' *-'