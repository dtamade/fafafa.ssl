#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_skip_local_guard_option"
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
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b149 skip local guard batch option contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_DEFAULT="b149_skip_local_guard_default_$$"
OUT_DEFAULT_REL="$WORK_REL/reports/default.md"
OUT_DEFAULT="$ROOT_DIR/$OUT_DEFAULT_REL"
DEFAULT_LOCAL_GUARD_LOG="$ROOT_DIR/test-reports/wave_c_b144_local_guard_batch_${RUN_DEFAULT}.b149.log"

rm -f "$OUT_DEFAULT" "$DEFAULT_LOCAL_GUARD_LOG"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_DEFAULT" \
  --output "$OUT_DEFAULT_REL" >/dev/null)

[[ -f "$OUT_DEFAULT" ]] || fail "default run should generate b149 report"
[[ -f "$DEFAULT_LOCAL_GUARD_LOG" ]] || fail "default run should generate local guard batch log"
assert_contains "$OUT_DEFAULT" "| B144C local guard skip-matrix batch |"

RUN_SKIP="b149_skip_local_guard_skip_$$"
OUT_SKIP_REL="$WORK_REL/reports/skip.md"
OUT_SKIP="$ROOT_DIR/$OUT_SKIP_REL"
SKIP_LOCAL_GUARD_LOG="$ROOT_DIR/test-reports/wave_c_b144_local_guard_batch_${RUN_SKIP}.b149.log"

rm -f "$OUT_SKIP" "$SKIP_LOCAL_GUARD_LOG"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_SKIP" \
  --output "$OUT_SKIP_REL" \
  --skip-local-guard-batch >/dev/null)

[[ -f "$OUT_SKIP" ]] || fail "skip run should generate b149 report"
[[ ! -f "$SKIP_LOCAL_GUARD_LOG" ]] || fail "skip run should not generate local guard batch log"
assert_contains "$OUT_SKIP" "| B144C local guard skip-matrix batch | SKIP | <none> | <none> |"

echo "[PASS] wave c b149 skip local guard batch option contract passed"
