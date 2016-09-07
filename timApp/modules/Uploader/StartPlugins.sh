#!/bin/bash
docker pull villet/uploaderplugin:latest
docker run -p 41419:41419 --rm villet/uploaderplugin /StartAll.sh
