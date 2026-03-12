#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_only_skip_semantics"
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
    sed -n '1,280p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b149 only+skip option semantics contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_only_skip_semantics_$$"
OUT_REL="$WORK_REL/reports/only_skip.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"
LOCAL_GUARD_LOG="$ROOT_DIR/test-reports/wave_c_b144_local_guard_batch_${RUN_ID}.b149.log"

rm -f "$OUT_FILE" "$LOCAL_GUARD_LOG"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --output "$OUT_REL" \
  --skip-docs-governance \
  --skip-local-guard-batch \
  --only-platform-path-check-dryrun >/dev/null)

[[ -f "$OUT_FILE" ]] || fail "combo run should generate b149 report"
[[ ! -f "$LOCAL_GUARD_LOG" ]] || fail "combo run should not generate local guard batch log"
assert_contains "$OUT_FILE" "| B144C local guard skip-matrix batch | SKIP | <none> | <none> |"
assert_contains "$OUT_FILE" "- b144c_local_guard_bundle_mode: SKIPPED"
assert_contains "$OUT_FILE" "- b144c_local_guard_option_resolution: SKIP_LOCAL_GUARD_BATCH_ONLY_FLAG_IGNORED"

echo "[PASS] wave c b149 only+skip option semantics contract passed"
