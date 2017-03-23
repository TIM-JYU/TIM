#!/usr/bin/env bash
# Compiles TypeScript files and minifies and packs them.
# Required when USE_OPTIMIZED_JS is set to True in Flask app config.

(cd timApp/static/scripts && ./compile_typescript.sh)
(cd timApp/static/scripts && ./optimize_js.sh)
