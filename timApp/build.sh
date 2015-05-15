#!/bin/bash

docker build --tag=tim:$(../get_latest_date.sh) .
