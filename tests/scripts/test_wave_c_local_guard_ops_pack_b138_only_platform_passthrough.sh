#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_local_guard_ops_pack.sh"
WORK_REL="tmp/test_wave_c_b144_b138_only_platform_passthrough"
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
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b144 b138 only-platform passthrough contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ONLY="b144_only_platform_$$"
OUT_ONLY_REL="$WORK_REL/reports/ops_only.md"
OUT_ONLY="$ROOT_DIR/$OUT_ONLY_REL"
B138_REPORT="$WORK_DIR/reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ONLY}.md"
B129_REPORT="$WORK_DIR/reports/wave_c_b129_oncall_check_${RUN_ONLY}.md"
B125_REPORT="$WORK_DIR/reports/wave_c_b125_local_guard_bundle_${RUN_ONLY}.md"
B125A_LOG="$WORK_DIR/reports/wave_c_b125_platform_path_checks_${RUN_ONLY}.log"
B123_REPORT="$WORK_DIR/reports/wave_c_b123_local_first_continuity_${RUN_ONLY}.md"
B124_REPORT="$WORK_DIR/reports/wave_c_b124_local_drift_watch_${RUN_ONLY}.md"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ONLY" \
  --reports-dir "$WORK_REL/reports" \
  --output "$OUT_ONLY_REL" \
  --only-platform-path-check-dryrun >/dev/null)

[[ -f "$OUT_ONLY" ]] || fail "only mode should generate ops pack report"
[[ -f "$B138_REPORT" ]] || fail "only mode should generate b138 report"
[[ -f "$B129_REPORT" ]] || fail "only mode should generate b129 report"
[[ -f "$B125_REPORT" ]] || fail "only mode should generate b125 report"
[[ -f "$B125A_LOG" ]] || fail "only mode should generate b125 platform log"
[[ ! -f "$B123_REPORT" ]] || fail "only mode should not generate b123 report"
[[ ! -f "$B124_REPORT" ]] || fail "only mode should not generate b124 report"

assert_contains "$OUT_ONLY" "| b138_local_first_bundle_mode | PLATFORM_ONLY |"
assert_contains "$B125_REPORT" "| B123 local continuity | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$B125_REPORT" "| B124 local drift watch | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$B129_REPORT" "| b125_platform_path_checks_state | PASS | PASS |"

echo "[PASS] wave c b144 b138 only-platform passthrough contract passed"
