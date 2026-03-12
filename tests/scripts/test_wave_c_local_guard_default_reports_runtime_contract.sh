#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REPORTS_DIR="$ROOT_DIR/tmp/wave_c_local_guard_reports"
LEGACY_DIR="$ROOT_DIR/test-reports"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave c local guard default reports runtime contract"

RUN_ONCALL="wave_c_local_guard_oncall_default_$$"
RUN_CONSIST="wave_c_local_guard_consistency_default_$$"
RUN_CLEAN="wave_c_local_guard_cleanup_default_$$"
RUN_SNAPSHOT="wave_c_local_guard_snapshot_default_$$"
RUN_STATUS="wave_c_local_guard_status_default_$$"

rm -f "$REPORTS_DIR/wave_c_b129_oncall_check_${RUN_ONCALL}.md"       "$REPORTS_DIR/wave_c_b125_local_guard_bundle_${RUN_ONCALL}.md"       "$REPORTS_DIR/wave_c_b125_platform_path_checks_${RUN_ONCALL}.log"       "$LEGACY_DIR/wave_c_b129_oncall_check_${RUN_ONCALL}.md"       "$LEGACY_DIR/wave_c_b125_local_guard_bundle_${RUN_ONCALL}.md"       "$LEGACY_DIR/wave_c_b125_platform_path_checks_${RUN_ONCALL}.log"       "$REPORTS_DIR/wave_c_b140_local_guard_consistency_${RUN_CONSIST}.md"       "$LEGACY_DIR/wave_c_b140_local_guard_consistency_${RUN_CONSIST}.md"       "$REPORTS_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_CLEAN}.md"       "$LEGACY_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_CLEAN}.md"       "$REPORTS_DIR/wave_c_b132_local_first_status_snapshot_${RUN_SNAPSHOT}.md"       "$LEGACY_DIR/wave_c_b132_local_first_status_snapshot_${RUN_SNAPSHOT}.md"       "$REPORTS_DIR/wave_c_b142_local_guard_status_${RUN_STATUS}.json"       "$LEGACY_DIR/wave_c_b142_local_guard_status_${RUN_STATUS}.json"

(cd "$ROOT_DIR" && bash scripts/run_wave_c_local_guard_oncall_check.sh --run-id "$RUN_ONCALL" --quiet --only-platform-path-check-dryrun >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b129_oncall_check_${RUN_ONCALL}.md" ]] || fail "default oncall report should be written under tmp/wave_c_local_guard_reports"
[[ -f "$REPORTS_DIR/wave_c_b125_local_guard_bundle_${RUN_ONCALL}.md" ]] || fail "default bundle report should be written under tmp/wave_c_local_guard_reports"
[[ -f "$REPORTS_DIR/wave_c_b125_platform_path_checks_${RUN_ONCALL}.log" ]] || fail "default bundle platform log should be written under tmp/wave_c_local_guard_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b129_oncall_check_${RUN_ONCALL}.md" ]] || fail "default oncall report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b125_local_guard_bundle_${RUN_ONCALL}.md" ]] || fail "default bundle report should no longer be written under test-reports"

(cd "$ROOT_DIR" && bash scripts/check_wave_c_local_guard_consistency.sh --run-id "$RUN_CONSIST" >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b140_local_guard_consistency_${RUN_CONSIST}.md" ]] || fail "default consistency report should be written under tmp/wave_c_local_guard_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b140_local_guard_consistency_${RUN_CONSIST}.md" ]] || fail "default consistency report should no longer be written under test-reports"

(cd "$ROOT_DIR" && bash scripts/cleanup_wave_c_local_guard_reports.sh --run-id "$RUN_CLEAN" >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_CLEAN}.md" ]] || fail "default cleanup-plan report should be written under tmp/wave_c_local_guard_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_CLEAN}.md" ]] || fail "default cleanup-plan report should no longer be written under test-reports"

(cd "$ROOT_DIR" && bash scripts/generate_wave_c_local_first_status_snapshot.sh --run-id "$RUN_SNAPSHOT" >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b132_local_first_status_snapshot_${RUN_SNAPSHOT}.md" ]] || fail "default snapshot report should be written under tmp/wave_c_local_guard_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b132_local_first_status_snapshot_${RUN_SNAPSHOT}.md" ]] || fail "default snapshot report should no longer be written under test-reports"

(cd "$ROOT_DIR" && bash scripts/export_wave_c_local_guard_status_json.sh --run-id "$RUN_STATUS" >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b142_local_guard_status_${RUN_STATUS}.json" ]] || fail "default status json should be written under tmp/wave_c_local_guard_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b142_local_guard_status_${RUN_STATUS}.json" ]] || fail "default status json should no longer be written under test-reports"

if ! rg -F --quiet '"consistency_report": "tmp/wave_c_local_guard_reports/' "$REPORTS_DIR/wave_c_b142_local_guard_status_${RUN_STATUS}.json"; then
  sed -n '1,220p' "$REPORTS_DIR/wave_c_b142_local_guard_status_${RUN_STATUS}.json" || true
  fail "status export should read latest consistency report from tmp/wave_c_local_guard_reports"
fi

echo "[PASS] wave c local guard default reports runtime contract passed"
