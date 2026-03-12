#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_local_guard_ops_pack.sh"
WORK_REL="tmp/test_wave_c_b144_b138_platform_skip_passthrough"
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
    sed -n '1,220p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b144 b138 platform path-check skip passthrough contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_DEFAULT="b144_platform_default_$$"
DEFAULT_OUT_REL="$WORK_REL/reports/ops_default.md"
DEFAULT_OUT="$ROOT_DIR/$DEFAULT_OUT_REL"
DEFAULT_B138_REPORT="$ROOT_DIR/test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_DEFAULT}.md"
DEFAULT_B129_REPORT="$ROOT_DIR/test-reports/wave_c_b129_oncall_check_${RUN_DEFAULT}.md"
DEFAULT_B125_REPORT="$ROOT_DIR/test-reports/wave_c_b125_local_guard_bundle_${RUN_DEFAULT}.md"
DEFAULT_B125_PLATFORM_LOG="$ROOT_DIR/test-reports/wave_c_b125_platform_path_checks_${RUN_DEFAULT}.log"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_DEFAULT" \
  --output "$DEFAULT_OUT_REL" >/dev/null)

[[ -f "$DEFAULT_OUT" ]] || fail "default run should generate ops pack report"
[[ -f "$DEFAULT_B138_REPORT" ]] || fail "default run should generate b138 report"
[[ -f "$DEFAULT_B129_REPORT" ]] || fail "default run should generate b129 report"
[[ -f "$DEFAULT_B125_REPORT" ]] || fail "default run should generate b125 report"
[[ -f "$DEFAULT_B125_PLATFORM_LOG" ]] || fail "default run should generate b125 platform path-check log"
assert_contains "$DEFAULT_OUT" "| b138_platform_path_checks_mode | ENABLED |"
assert_contains "$DEFAULT_B129_REPORT" "| b125_platform_path_checks_state |"

RUN_SKIP="b144_platform_skip_$$"
SKIP_OUT_REL="$WORK_REL/reports/ops_skip.md"
SKIP_OUT="$ROOT_DIR/$SKIP_OUT_REL"
SKIP_B138_REPORT="$ROOT_DIR/test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_SKIP}.md"
SKIP_B129_REPORT="$ROOT_DIR/test-reports/wave_c_b129_oncall_check_${RUN_SKIP}.md"
SKIP_B125_REPORT="$ROOT_DIR/test-reports/wave_c_b125_local_guard_bundle_${RUN_SKIP}.md"
SKIP_B125_PLATFORM_LOG="$ROOT_DIR/test-reports/wave_c_b125_platform_path_checks_${RUN_SKIP}.log"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_SKIP" \
  --output "$SKIP_OUT_REL" \
  --skip-platform-path-checks-dryrun >/dev/null)

[[ -f "$SKIP_OUT" ]] || fail "skip run should generate ops pack report"
[[ -f "$SKIP_B138_REPORT" ]] || fail "skip run should generate b138 report"
[[ -f "$SKIP_B129_REPORT" ]] || fail "skip run should generate b129 report"
[[ -f "$SKIP_B125_REPORT" ]] || fail "skip run should generate b125 report"
[[ ! -f "$SKIP_B125_PLATFORM_LOG" ]] || fail "skip run should not generate b125 platform path-check log"
assert_contains "$SKIP_OUT" "| b138_platform_path_checks_mode | SKIPPED |"
assert_contains "$SKIP_B125_REPORT" "| B125A platform path-check dry-run batch | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$SKIP_B129_REPORT" "| b125_platform_path_checks_state | SKIPPED | PASS |"

echo "[PASS] wave c b144 b138 platform path-check skip passthrough contract passed"
