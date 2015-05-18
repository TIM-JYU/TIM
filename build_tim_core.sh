#!/bin/bash

(cd timApp && ./build.sh)
(cd Ephemeral && ./install.sh)
(cd local_nginx && ./build_nginx.sh)
