#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_local_guard_batch_integration"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    echo "[INFO] top of output ($file):"
    sed -n '1,240p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b149 local guard batch integration contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_local_guard_batch_$$"
OUT_REL="$WORK_REL/reports/b149.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"
B149_LOCAL_GUARD_LOG="$ROOT_DIR/test-reports/wave_c_b144_local_guard_batch_${RUN_ID}.b149.log"

rm -f "$OUT_FILE" "$B149_LOCAL_GUARD_LOG"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --output "$OUT_REL" >/dev/null)

[[ -f "$OUT_FILE" ]] || fail "b149 run should generate submission bundle report"
[[ -f "$B149_LOCAL_GUARD_LOG" ]] || fail "b149 run should generate local guard batch log"
assert_contains "$OUT_FILE" "B144C local guard skip-matrix batch"
assert_contains "$B149_LOCAL_GUARD_LOG" "[PASS] wave c b144 reports-dir + platform skip matrix batch contract passed"

echo "[PASS] wave c b149 local guard batch integration contract passed"
