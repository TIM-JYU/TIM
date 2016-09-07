#!/bin/bash

(cd timApp/modules/cs && ./startPlugins.sh)
(cd timApp/modules/svn && ./startPlugins.sh)
(cd timApp/modules/Haskell && ./startPlugins.sh)
(cd timApp/modules/Uploader/ && ./startPlugins.sh)
(cd timApp/modules/imagex && ./startPlugins.sh)
