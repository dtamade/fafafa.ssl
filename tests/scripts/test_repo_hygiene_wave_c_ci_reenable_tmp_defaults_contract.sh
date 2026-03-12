#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
B137="$ROOT_DIR/scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh"
B143="$ROOT_DIR/scripts/check_wave_c_local_guard_alert_thresholds.sh"
B146="$ROOT_DIR/scripts/prepare_wave_c_ci_reenable_submission_pack.sh"
B147="$ROOT_DIR/scripts/check_wave_c_ci_reenable_submission_pack.sh"
B148="$ROOT_DIR/scripts/generate_wave_c_ci_reenable_approval_brief.sh"
B149="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,260p' "$file" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] repo hygiene wave c ci re-enable tmp defaults contract"

assert_contains "$B137" 'LOCAL_GUARD_REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B137" 'REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"'
assert_contains "$B137" 'ONCALL_REPORT="$(ls -1t "$LOCAL_GUARD_REPORTS_DIR"/wave_c_b129_oncall_check_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B137" 'SNAPSHOT_REPORT="$(ls -1t "$LOCAL_GUARD_REPORTS_DIR"/wave_c_b132_local_first_status_snapshot_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B137" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_ID}.md"'

assert_contains "$B143" 'LOCAL_GUARD_REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B143" 'REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"'
assert_contains "$B143" 'INPUT_JSON="$(ls -1t "$LOCAL_GUARD_REPORTS_DIR"/wave_c_b142_local_guard_status_*.json 2>/dev/null | head -1 || true)"'
assert_contains "$B143" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.md"'

assert_contains "$B146" 'LOCAL_GUARD_REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"'
assert_contains "$B146" 'REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"'
assert_contains "$B146" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.md"'
assert_contains "$B146" 'latest_packet="$(ls -1t "$REPORTS_DIR"/wave_c_b137_pre_ci_reenable_packet_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B146" 'latest_fullgate="$(ls -1t "$LOCAL_GUARD_REPORTS_DIR"/wave_c_b138_pre_ci_reenable_full_gate_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B146" 'latest_status_json="$(ls -1t "$LOCAL_GUARD_REPORTS_DIR"/wave_c_b142_local_guard_status_*.json 2>/dev/null | head -1 || true)"'
assert_contains "$B146" 'latest_alert="$(ls -1t "$REPORTS_DIR"/wave_c_b143_alert_thresholds_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B146" 'latest_ops_pack="$(ls -1t "$LOCAL_GUARD_REPORTS_DIR"/wave_c_b144_local_guard_ops_pack_*.md 2>/dev/null | head -1 || true)"'

assert_contains "$B147" 'REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"'
assert_contains "$B147" 'INPUT_FILE="$(ls -1t "$REPORTS_DIR"/wave_c_b146_ci_reenable_submission_pack_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B147" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.md"'

assert_contains "$B148" 'REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"'
assert_contains "$B148" 'INPUT_FILE="$(ls -1t "$REPORTS_DIR"/wave_c_b146_ci_reenable_submission_pack_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B148" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.md"'
assert_contains "$B148" 'CHECK_FILE="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.md"'

assert_contains "$B149" 'DEFAULT_REPORTS_DIR="tmp/wave_c_ci_reenable_reports"'
assert_contains "$B149" 'REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'
assert_contains "$B149" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b149_ci_reenable_submission_bundle_${RUN_ID}.md"'
assert_contains "$B149" 'b146_report="$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.md"'
assert_contains "$B149" 'b147_report="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.md"'
assert_contains "$B149" 'b148_report="$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.md"'
assert_contains "$B149" 'b146_log="$REPORTS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.b149.log"'
assert_contains "$B149" 'b147_log="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.b149.log"'
assert_contains "$B149" 'b148_log="$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.b149.log"'
assert_contains "$B149" 'b144c_local_guard_batch_log="$REPORTS_DIR/wave_c_b144_local_guard_batch_${RUN_ID}.b149.log"'
assert_contains "$B149" 'b149d_docs_governance_log="$REPORTS_DIR/wave_c_docs_governance_batch_${RUN_ID}.b149.log"'


echo "[PASS] repo hygiene wave c ci re-enable tmp defaults contract passed"
