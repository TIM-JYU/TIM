#!/usr/bin/env bash

set -euo pipefail

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

REG_DIR="$DIR/rust/cargo-cache/registry"
mkdir -p "$REG_DIR"

# We need an image with both nightly Rust and Python installed. The cs3 image has both, so we use that.
# The cs3 Python version must be the same as TIM image Python version.
# Difference in Python patch version is probably ok, such as 3.9.5 vs 3.9.6.
docker run \
  --rm \
  --user "$(id -u)":"$(id -g)" \
  -v "$DIR/rust/tim-core":/app \
  -v "$REG_DIR":/cargo/registry \
  -w /app \
  timimages/cs3:focal \
  cargo b --release

cp "$DIR/rust/tim-core/target/release/libtim_core.so" "$DIR/timApp/rust_ext/tim_core.so"

mv timApp/rust_ext/tim_core.pyi timApp/rust_ext/tim_core_current.pyi && \
  ./dc run --rm --user "$(id -u)":"$(id -g)" tim stubgen -m rust_ext.tim_core -o . && \
  mv timApp/rust_ext/tim_core.pyi timApp/rust_ext/tim_core_template.pyi && \
  mv timApp/rust_ext/tim_core_current.pyi timApp/rust_ext/tim_core.pyi
