#!/usr/bin/env bash

./run_command_workdir.sh timApp/docs /bin/bash -c "sphinx-apidoc -o . .. && sphinx-build . _build"
