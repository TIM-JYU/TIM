#!/bin/bash
echo "`date +%Y%m%d-%H%M%S`: docker $@" >> /tmp/log/log.txt
docker $@
 