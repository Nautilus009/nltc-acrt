#!/usr/bin/env bash
set -euo pipefail

# Example env (adjust as needed)
export ACRT_HOME="${ACRT_HOME:-/CCA/DEVELOP/$USER/BUILD/DEV/classic_ica}"
export BUILD_ALM_PATH_BB="${BUILD_ALM_PATH_BB:-/CCA/BUILD/BUILDDEV/DEV/classic_ica}"
export BUILD_LOCAL_PATH_BB="${BUILD_LOCAL_PATH_BB:-/CCA/DEVELOP/$USER/BUILD/DEV/classic_ica}"

src="${1:?Usage: $0 <file.cob|file.pco|file.inc>}"
python3 "$(cd "$(dirname "$0")/.." && pwd)/acrt.py" "$src"
