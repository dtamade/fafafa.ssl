#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_all_module_tests.sh"
WORK_DIR="$ROOT_DIR/tmp/test_run_all_module_tests_parallel_output_isolation"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] run_all_module_tests parallel output isolation contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

LOG1="$WORK_DIR/run1.log"
LOG2="$WORK_DIR/run2.log"
BIN1="$WORK_DIR/bin1"
BIN2="$WORK_DIR/bin2"
UNITS1="$WORK_DIR/units1"
UNITS2="$WORK_DIR/units2"

(
  cd "$ROOT_DIR"
  FAFAFA_TEST_BIN_DIR="$BIN1" \
  FAFAFA_FPC_UNIT_OUTPUT_DIR="$UNITS1" \
  bash "$SCRIPT" --modules PKCS7 >"$LOG1" 2>&1
) &
PID1=$!

(
  cd "$ROOT_DIR"
  FAFAFA_TEST_BIN_DIR="$BIN2" \
  FAFAFA_FPC_UNIT_OUTPUT_DIR="$UNITS2" \
  bash "$SCRIPT" --modules PKCS7 >"$LOG2" 2>&1
) &
PID2=$!

RC1=0
RC2=0
wait "$PID1" || RC1=$?
wait "$PID2" || RC2=$?

[[ "$RC1" -eq 0 ]] || {
  sed -n '1,200p' "$LOG1" || true
  fail "parallel run1 should succeed"
}

[[ "$RC2" -eq 0 ]] || {
  sed -n '1,200p' "$LOG2" || true
  fail "parallel run2 should succeed"
}

if ! rg -F --quiet "Binary output dir: $BIN1" "$LOG1"; then
  sed -n '1,200p' "$LOG1" || true
  fail "run1 should report isolated binary output dir"
fi

if ! rg -F --quiet "Binary output dir: $BIN2" "$LOG2"; then
  sed -n '1,200p' "$LOG2" || true
  fail "run2 should report isolated binary output dir"
fi

if ! rg -F --quiet "FPC unit output dir: $UNITS1" "$LOG1"; then
  sed -n '1,200p' "$LOG1" || true
  fail "run1 should report isolated unit output dir"
fi

if ! rg -F --quiet "FPC unit output dir: $UNITS2" "$LOG2"; then
  sed -n '1,200p' "$LOG2" || true
  fail "run2 should report isolated unit output dir"
fi

[[ -f "$BIN1/test_p2_pkcs7" ]] || fail "run1 binary output should contain test_p2_pkcs7"
[[ -f "$BIN2/test_p2_pkcs7" ]] || fail "run2 binary output should contain test_p2_pkcs7"

REPORT1="$(grep -E "^详细报告:" "$LOG1" | sed -E 's/^详细报告:[[:space:]]*//' | tail -n1)"
REPORT2="$(grep -E "^详细报告:" "$LOG2" | sed -E 's/^详细报告:[[:space:]]*//' | tail -n1)"
[[ -n "$REPORT1" ]] || fail "run1 report path should not be empty"
[[ -n "$REPORT2" ]] || fail "run2 report path should not be empty"
[[ "$REPORT1" != "$REPORT2" ]] || fail "parallel runs should not share same report file path"
[[ -f "$REPORT1" ]] || fail "run1 report file should exist"
[[ -f "$REPORT2" ]] || fail "run2 report file should exist"
[[ "$REPORT1" == "$ROOT_DIR"/tmp/* ]] || fail "run1 report path should default under tmp/"
[[ "$REPORT2" == "$ROOT_DIR"/tmp/* ]] || fail "run2 report path should default under tmp/"

echo "[PASS] run_all_module_tests parallel output isolation contract"
