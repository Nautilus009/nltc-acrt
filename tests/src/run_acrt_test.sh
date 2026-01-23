#!/usr/bin/env sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
ROOT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)

export ACRT_HOME="$ROOT_DIR"
export BUILD_ALM_PATH_BB="$ROOT_DIR/tests/master"
export BUILD_LOCAL_PATH_BB="$ROOT_DIR/tests/local"

#export ACRT_EXCLUDE="icd*.pco;ica_check*_subs_active.cob;"
LOG_DIR="$ROOT_DIR/tests/log"
LOG_FILE="$LOG_DIR/run_acrt_test.log"
EXP_FILE="$LOG_DIR/run_acrt_test.txt"
mkdir -p "$LOG_DIR"
: > "$LOG_FILE"
set +e
echo "+ python3 acrt.py src/ica_check_if_subs_active.cob" >> "$LOG_FILE"
python3 "$ROOT_DIR/src/acrt.py" "$BUILD_LOCAL_PATH_BB/src/ica_check_if_subs_active.cob" >> "$LOG_FILE"
echo "+ python3 acrt.py src/demo_ica_chk_cob_regression.cob" >> "$LOG_FILE"
python3 "$ROOT_DIR/src/acrt.py" "$BUILD_LOCAL_PATH_BB/src/demo_ica_chk_cob_regression.cob" >> "$LOG_FILE"
set -e

cat "$LOG_FILE"
diff -sb "$LOG_FILE" "$EXP_FILE"
TMP_A=$(mktemp)
TMP_B=$(mktemp)
sed -E -e 's/on [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/on <timestamp>/' \
       -e 's/at: [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/at: <timestamp>/' \
  "$BUILD_LOCAL_PATH_BB/target/obj/ica_check_if_subs_active.acrt" > "$TMP_A"
sed -E -e 's/on [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/on <timestamp>/' \
       -e 's/at: [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/at: <timestamp>/' \
  "$LOG_DIR/ica_check_if_subs_active.acrt" > "$TMP_B"
diff -sb "$TMP_A" "$TMP_B"

sed -E -e 's/on [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/on <timestamp>/' \
       -e 's/at: [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/at: <timestamp>/' \
  "$BUILD_LOCAL_PATH_BB/target/obj/demo_ica_chk_cob_regression.acrt" > "$TMP_A"
sed -E -e 's/on [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/on <timestamp>/' \
       -e 's/at: [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/at: <timestamp>/' \
  "$LOG_DIR/demo_ica_chk_cob_regression.acrt" > "$TMP_B"
diff -sb "$TMP_A" "$TMP_B"
rm -f "$TMP_A" "$TMP_B"
