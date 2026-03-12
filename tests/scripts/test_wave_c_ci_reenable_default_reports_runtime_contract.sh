#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REPORTS_DIR="$ROOT_DIR/tmp/wave_c_ci_reenable_reports"
LOCAL_GUARD_DIR="$ROOT_DIR/tmp/wave_c_local_guard_reports"
LEGACY_DIR="$ROOT_DIR/test-reports"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave c ci re-enable default reports runtime contract"

RUN_PACKET="wave_c_ci_reenable_packet_default_$$"
RUN_ALERT_STATUS="wave_c_ci_reenable_alert_status_default_$$"
RUN_ALERT="wave_c_ci_reenable_alert_default_$$"
RUN_BUNDLE="wave_c_ci_reenable_bundle_default_$$"

rm -f \
  "$REPORTS_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_PACKET}.md" \
  "$LEGACY_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_PACKET}.md" \
  "$LOCAL_GUARD_DIR/wave_c_b142_local_guard_status_${RUN_ALERT_STATUS}.json" \
  "$LEGACY_DIR/wave_c_b142_local_guard_status_${RUN_ALERT_STATUS}.json" \
  "$REPORTS_DIR/wave_c_b143_alert_thresholds_${RUN_ALERT}.md" \
  "$LEGACY_DIR/wave_c_b143_alert_thresholds_${RUN_ALERT}.md" \
  "$REPORTS_DIR/wave_c_b149_ci_reenable_submission_bundle_${RUN_BUNDLE}.md" \
  "$LEGACY_DIR/wave_c_b149_ci_reenable_submission_bundle_${RUN_BUNDLE}.md" \
  "$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.md" \
  "$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.md" \
  "$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.md" \
  "$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.b149.log" \
  "$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.b149.log" \
  "$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.b149.log" \
  "$LEGACY_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.md" \
  "$LEGACY_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.md" \
  "$LEGACY_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.md" \
  "$LEGACY_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.b149.log" \
  "$LEGACY_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.b149.log" \
  "$LEGACY_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.b149.log"

(cd "$ROOT_DIR" && bash scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh --run-id "$RUN_PACKET" >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_PACKET}.md" ]] || fail "default B137 report should be written under tmp/wave_c_ci_reenable_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_PACKET}.md" ]] || fail "default B137 report should no longer be written under test-reports"

(cd "$ROOT_DIR" && bash scripts/export_wave_c_local_guard_status_json.sh --run-id "$RUN_ALERT_STATUS" >/dev/null)
[[ -f "$LOCAL_GUARD_DIR/wave_c_b142_local_guard_status_${RUN_ALERT_STATUS}.json" ]] || fail "default B142 json should be written under tmp/wave_c_local_guard_reports"

(cd "$ROOT_DIR" && bash scripts/check_wave_c_local_guard_alert_thresholds.sh --run-id "$RUN_ALERT" >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b143_alert_thresholds_${RUN_ALERT}.md" ]] || fail "default B143 report should be written under tmp/wave_c_ci_reenable_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b143_alert_thresholds_${RUN_ALERT}.md" ]] || fail "default B143 report should no longer be written under test-reports"

(cd "$ROOT_DIR" && bash scripts/run_wave_c_ci_reenable_submission_bundle.sh --run-id "$RUN_BUNDLE" --skip-local-guard-batch --skip-docs-governance >/dev/null)
[[ -f "$REPORTS_DIR/wave_c_b149_ci_reenable_submission_bundle_${RUN_BUNDLE}.md" ]] || fail "default B149 report should be written under tmp/wave_c_ci_reenable_reports"
[[ -f "$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.md" ]] || fail "default B146 report should be written under tmp/wave_c_ci_reenable_reports"
[[ -f "$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.md" ]] || fail "default B147 report should be written under tmp/wave_c_ci_reenable_reports"
[[ -f "$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.md" ]] || fail "default B148 report should be written under tmp/wave_c_ci_reenable_reports"
[[ -f "$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.b149.log" ]] || fail "default B149 B146 log should be written under tmp/wave_c_ci_reenable_reports"
[[ -f "$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.b149.log" ]] || fail "default B149 B147 log should be written under tmp/wave_c_ci_reenable_reports"
[[ -f "$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.b149.log" ]] || fail "default B149 B148 log should be written under tmp/wave_c_ci_reenable_reports"
[[ ! -f "$LEGACY_DIR/wave_c_b149_ci_reenable_submission_bundle_${RUN_BUNDLE}.md" ]] || fail "default B149 report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.md" ]] || fail "default B146 report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.md" ]] || fail "default B147 report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.md" ]] || fail "default B148 report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_BUNDLE}.b149.log" ]] || fail "default B149 B146 log should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b147_submission_pack_check_${RUN_BUNDLE}.b149.log" ]] || fail "default B149 B147 log should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_BUNDLE}.b149.log" ]] || fail "default B149 B148 log should no longer be written under test-reports"

echo "[PASS] wave c ci re-enable default reports runtime contract passed"
