#!/usr/bin/env sh
set -eu

# Run the modular entrypoint from src/ to avoid the top-level acrt.py shadowing the package.
cd "$(dirname "$0")/../src"

export ACRT_HOME=".."
export BUILD_ALM_PATH_BB="../tests/master"
export BUILD_LOCAL_PATH_BB="../tests/local"

python3 acrt.py ../tests/src/ica_check_if_subs_active.cob
