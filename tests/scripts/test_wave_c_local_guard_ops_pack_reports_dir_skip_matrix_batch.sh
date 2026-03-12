#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_local_guard_ops_pack.sh"
WORK_REL="tmp/test_wave_c_b144_reports_dir_skip_matrix_batch"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
ONLY_PLATFORM_PATH_CHECK_DRYRUN=false

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

while [[ $# -gt 0 ]]; do
  case "$1" in
    --only-platform-path-check-dryrun)
      ONLY_PLATFORM_PATH_CHECK_DRYRUN=true
      shift
      ;;
    *)
      echo "[FAIL] Unknown option: $1"
      exit 1
      ;;
  esac
done

echo "[TEST] wave c b144 reports-dir + platform skip matrix batch contract"

rm -rf "$WORK_DIR"
mkdir -p "$REPORTS_DIR"

RUN_DEFAULT="b144_reports_dir_default_$$"
DEFAULT_OUT_REL="$REPORTS_REL/ops_default.md"
DEFAULT_OUT="$ROOT_DIR/$DEFAULT_OUT_REL"
DEFAULT_B138_REPORT="$REPORTS_DIR/wave_c_b138_pre_ci_reenable_full_gate_${RUN_DEFAULT}.md"
DEFAULT_B129_REPORT="$REPORTS_DIR/wave_c_b129_oncall_check_${RUN_DEFAULT}.md"
DEFAULT_B125_REPORT="$REPORTS_DIR/wave_c_b125_local_guard_bundle_${RUN_DEFAULT}.md"
DEFAULT_B125_PLATFORM_LOG="$REPORTS_DIR/wave_c_b125_platform_path_checks_${RUN_DEFAULT}.log"
DEFAULT_B123_REPORT="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_DEFAULT}.md"
DEFAULT_B124_REPORT="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_DEFAULT}.md"
DEFAULT_B123_LOG="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_DEFAULT}.log"
DEFAULT_B124_LOG="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_DEFAULT}.log"

rm -f "$DEFAULT_OUT" "$DEFAULT_B138_REPORT" "$DEFAULT_B129_REPORT" "$DEFAULT_B125_REPORT" "$DEFAULT_B125_PLATFORM_LOG" \
  "$DEFAULT_B123_REPORT" "$DEFAULT_B124_REPORT" "$DEFAULT_B123_LOG" "$DEFAULT_B124_LOG"
default_cmd=(bash "$SCRIPT"
  --run-id "$RUN_DEFAULT"
  --reports-dir "$REPORTS_REL"
  --output "$DEFAULT_OUT_REL")
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
  default_cmd+=(--only-platform-path-check-dryrun)
fi
(cd "$ROOT_DIR" && "${default_cmd[@]}" >/dev/null)

[[ -f "$DEFAULT_OUT" ]] || fail "default run should generate ops pack report under custom reports-dir"
[[ -f "$DEFAULT_B138_REPORT" ]] || fail "default run should generate b138 report under custom reports-dir"
[[ -f "$DEFAULT_B129_REPORT" ]] || fail "default run should generate b129 report under custom reports-dir"
[[ -f "$DEFAULT_B125_REPORT" ]] || fail "default run should generate b125 report under custom reports-dir"
[[ -f "$DEFAULT_B125_PLATFORM_LOG" ]] || fail "default run should generate b125 platform log under custom reports-dir"
assert_contains "$DEFAULT_OUT" "| b138_platform_path_checks_mode | ENABLED |"
assert_contains "$DEFAULT_B129_REPORT" "| b125_platform_path_checks_state |"
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
  [[ ! -f "$DEFAULT_B123_REPORT" ]] || fail "default only-mode should not generate b123 report"
  [[ ! -f "$DEFAULT_B124_REPORT" ]] || fail "default only-mode should not generate b124 report"
  [[ ! -f "$DEFAULT_B123_LOG" ]] || fail "default only-mode should not generate b123 log"
  [[ ! -f "$DEFAULT_B124_LOG" ]] || fail "default only-mode should not generate b124 log"
  assert_contains "$DEFAULT_OUT" "| b138_local_first_bundle_mode | PLATFORM_ONLY |"
  assert_contains "$DEFAULT_B125_REPORT" "| B123 local continuity | SKIP | SKIPPED | <none> | <none> |"
  assert_contains "$DEFAULT_B125_REPORT" "| B124 local drift watch | SKIP | SKIPPED | <none> | <none> |"
else
  assert_contains "$DEFAULT_OUT" "| b138_local_first_bundle_mode | FULL |"
fi

RUN_SKIP="b144_reports_dir_skip_$$"
SKIP_OUT_REL="$REPORTS_REL/ops_skip.md"
SKIP_OUT="$ROOT_DIR/$SKIP_OUT_REL"
SKIP_B138_REPORT="$REPORTS_DIR/wave_c_b138_pre_ci_reenable_full_gate_${RUN_SKIP}.md"
SKIP_B129_REPORT="$REPORTS_DIR/wave_c_b129_oncall_check_${RUN_SKIP}.md"
SKIP_B125_REPORT="$REPORTS_DIR/wave_c_b125_local_guard_bundle_${RUN_SKIP}.md"
SKIP_B125_PLATFORM_LOG="$REPORTS_DIR/wave_c_b125_platform_path_checks_${RUN_SKIP}.log"
SKIP_B123_REPORT="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_SKIP}.md"
SKIP_B124_REPORT="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_SKIP}.md"
SKIP_B123_LOG="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_SKIP}.log"
SKIP_B124_LOG="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_SKIP}.log"

rm -f "$SKIP_OUT" "$SKIP_B138_REPORT" "$SKIP_B129_REPORT" "$SKIP_B125_REPORT" "$SKIP_B125_PLATFORM_LOG" \
  "$SKIP_B123_REPORT" "$SKIP_B124_REPORT" "$SKIP_B123_LOG" "$SKIP_B124_LOG"
skip_cmd=(bash "$SCRIPT"
  --run-id "$RUN_SKIP"
  --reports-dir "$REPORTS_REL"
  --output "$SKIP_OUT_REL"
  --skip-platform-path-checks-dryrun)
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
  skip_cmd+=(--only-platform-path-check-dryrun)
fi
(cd "$ROOT_DIR" && "${skip_cmd[@]}" >/dev/null)

[[ -f "$SKIP_OUT" ]] || fail "skip run should generate ops pack report under custom reports-dir"
[[ -f "$SKIP_B138_REPORT" ]] || fail "skip run should generate b138 report under custom reports-dir"
[[ -f "$SKIP_B129_REPORT" ]] || fail "skip run should generate b129 report under custom reports-dir"
[[ -f "$SKIP_B125_REPORT" ]] || fail "skip run should generate b125 report under custom reports-dir"
[[ ! -f "$SKIP_B125_PLATFORM_LOG" ]] || fail "skip run should not generate b125 platform path-check log"
assert_contains "$SKIP_OUT" "| b138_platform_path_checks_mode | SKIPPED |"
assert_contains "$SKIP_B125_REPORT" "| B125A platform path-check dry-run batch | SKIP | SKIPPED | <none> | <none> |"
assert_contains "$SKIP_B129_REPORT" "| b125_platform_path_checks_state | SKIPPED | PASS |"
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
  [[ ! -f "$SKIP_B123_REPORT" ]] || fail "skip only-mode should not generate b123 report"
  [[ ! -f "$SKIP_B124_REPORT" ]] || fail "skip only-mode should not generate b124 report"
  [[ ! -f "$SKIP_B123_LOG" ]] || fail "skip only-mode should not generate b123 log"
  [[ ! -f "$SKIP_B124_LOG" ]] || fail "skip only-mode should not generate b124 log"
  assert_contains "$SKIP_OUT" "| b138_local_first_bundle_mode | PLATFORM_ONLY |"
  assert_contains "$SKIP_B125_REPORT" "| B123 local continuity | SKIP | SKIPPED | <none> | <none> |"
  assert_contains "$SKIP_B125_REPORT" "| B124 local drift watch | SKIP | SKIPPED | <none> | <none> |"
else
  assert_contains "$SKIP_OUT" "| b138_local_first_bundle_mode | FULL |"
fi

echo "[PASS] wave c b144 reports-dir + platform skip matrix batch contract passed"
