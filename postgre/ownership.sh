#!/bin/bash

# Allows postgres user in postgre container to access
# volumes mapped outside of the container

chown -Rf postgres:postgres /var/lib/postgresql
chmod -R 700 /var/lib/postgresql

chown -Rf postgres:postgres /etc/postgresql
chmod -R 700 /etc/postgresql

chown -Rf postgres:postgres /var/log/postgresql
chmod -R 700 /var/log/postgresql
