#!/bin/bash
docker build --tag=timimages/tim:$(../get_latest_date.sh) $@ .
