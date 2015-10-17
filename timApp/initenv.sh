#!/bin/bash
export LANG=en_US.utf8
export LANGUAGE=en_US.utf8
locale-gen en_US.utf8
service postgresql start

# Wait until postgresql service is started
# Otherwise tim will give an error when trying to connect to database
service postgresql status | grep online
while [[ "$?" -eq 1 ]]; do
  sleep 1
  service postgresql status | grep online
done
