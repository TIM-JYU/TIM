#!/bin/sh
# Ajetaan docker yhden cs-ajon ajaksi ja liitet‰‰n siihen k‰ytt‰j‰n hakemisto hakemistoksi /home/me
docker run -t -i -v /opt/cs:/cs/:ro  -v /tmp/uhome/user/4d859744c28dbca8348fc24833ece03aa3050371f98a882bbd4b54e5da617114:/home/me/ cs3 /cs/rcmd.sh
