#!/bin/bash

(cd timApp && ./build.sh $@)
(cd Ephemeral/Dumbo && ./Build.sh $@)
(cd local_nginx && ./build_nginx.sh $@)

./create_network.sh
