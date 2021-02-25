#!/usr/bin/env bash

# Download scripts with:
# curl -s https://gitlab.com/tim-jyu/tim/-/raw/master/installscripts/download.sh | bash

SCRIPTS_URL_BASE=https://gitlab.com/tim-jyu/tim/-/raw/master/installscripts

curl -s "${SCRIPTS_URL_BASE}/checkargs.sh" > checkargs.sh
curl -s "${SCRIPTS_URL_BASE}/common.sh" > common.sh
curl -s "${SCRIPTS_URL_BASE}/rhel8.sh" > rhel8.sh
curl -s "${SCRIPTS_URL_BASE}/ubuntu.sh" > ubuntu.sh

chmod u+x checkargs.sh common.sh rhel8.sh ubuntu.sh

echo "Installation scripts downloaded. Now, run

sudo ./<distro>.sh <server's domain>

where supported distros are currently:

* rhel8
* ubuntu
"
