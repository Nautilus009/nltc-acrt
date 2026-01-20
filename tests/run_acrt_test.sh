#!/usr/bin/env sh
set -eu

export ACRT_HOME="."
export BUILD_ALM_PATH_BB="./tests/master/"
export BUILD_LOCAL_PATH_BB="./tests/local/"

python3 acrt.py tests/src/ica_check_if_subs_active.cob
