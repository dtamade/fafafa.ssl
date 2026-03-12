#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_only_platform_option"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b149 only-platform option contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ONLY="b149_only_platform_$$"
OUT_ONLY_REL="$WORK_REL/reports/only.md"
OUT_ONLY="$ROOT_DIR/$OUT_ONLY_REL"
ONLY_LOCAL_GUARD_LOG="$ROOT_DIR/test-reports/wave_c_b144_local_guard_batch_${RUN_ONLY}.b149.log"

rm -f "$OUT_ONLY" "$ONLY_LOCAL_GUARD_LOG"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ONLY" \
  --output "$OUT_ONLY_REL" \
  --skip-docs-governance \
  --only-platform-path-check-dryrun >/dev/null)

[[ -f "$OUT_ONLY" ]] || fail "only run should generate b149 report"
[[ -f "$ONLY_LOCAL_GUARD_LOG" ]] || fail "only run should generate local guard batch log"
assert_contains "$OUT_ONLY" "| B144C local guard skip-matrix batch |"
assert_contains "$OUT_ONLY" "- b144c_local_guard_bundle_mode: PLATFORM_ONLY"
assert_contains "$ONLY_LOCAL_GUARD_LOG" "[PASS] wave c b144 reports-dir + platform skip matrix batch contract passed"

echo "[PASS] wave c b149 only-platform option contract passed"
