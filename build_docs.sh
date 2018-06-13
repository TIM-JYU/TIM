#!/usr/bin/env bash

./run_command_workdir.sh python_docs /bin/bash -c "sphinx-apidoc -o . .. && sphinx-build . _build"
