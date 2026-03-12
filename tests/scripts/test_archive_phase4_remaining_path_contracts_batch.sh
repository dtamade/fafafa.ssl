#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REL_BASE="tmp/test_archive_phase4_remaining_path_batch"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if [[ -n "$pattern" ]] && ! rg -F --quiet "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    echo "[INFO] top of output ($file):"
    sed -n '1,120p' "$file" || true
    exit 1
  fi
}

run_path_case() {
  local case_id="$1"
  local cmd="$2"
  local rel_output="$3"
  local expected_pattern="$4"

  local root_output="$ROOT_DIR/$rel_output"
  local tmp_output="/tmp/$rel_output"

  rm -f "$root_output" "$tmp_output"
  mkdir -p "$(dirname "$root_output")"

  if ! (cd "$ROOT_DIR" && eval "$cmd" >/dev/null 2>&1); then
    fail "$case_id: command failed when executed from project root"
  fi

  [[ -f "$root_output" ]] || fail "$case_id: expected output not generated under project root"
  assert_contains "$root_output" "$expected_pattern"

  rm -f "$root_output" "$tmp_output"
  mkdir -p "$(dirname "$root_output")"

  if ! (cd /tmp && eval "$cmd" >/dev/null 2>&1); then
    fail "$case_id: command failed when executed from /tmp with relative inputs"
  fi

  [[ -f "$root_output" ]] || fail "$case_id: output should resolve under project root for relative --output"
  [[ ! -f "$tmp_output" ]] || fail "$case_id: output leaked into /tmp for relative --output"
  assert_contains "$root_output" "$expected_pattern"

  echo "[PASS] $case_id"
}

echo "[TEST] Archive Phase4 remaining drafts - path contracts (batch)"

run_path_case \
  "autofix_archive_audit_writeback_coverage_draft.sh" \
  "bash '$ROOT_DIR/scripts/autofix_archive_audit_writeback_coverage_draft.sh' --closure-gate-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md --autofix-id path_batch_autofix --output '$REL_BASE/autofix.md'" \
  "$REL_BASE/autofix.md" \
  "| autofix_status |"

run_path_case \
  "drill_archive_audit_linkage_rollback_playbook_draft.sh" \
  "bash '$ROOT_DIR/scripts/drill_archive_audit_linkage_rollback_playbook_draft.sh' --drill-id path_batch_drill --tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md --output '$REL_BASE/drill_playbook.md'" \
  "$REL_BASE/drill_playbook.md" \
  "| drill_status |"

run_path_case \
  "evaluate_archive_audit_dashboard_thresholds_draft.sh" \
  "bash '$ROOT_DIR/scripts/evaluate_archive_audit_dashboard_thresholds_draft.sh' --policy-id path_batch_eval --dashboard docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md --output '$REL_BASE/evaluate_thresholds.md'" \
  "$REL_BASE/evaluate_thresholds.md" \
  "| decision_status |"

run_path_case \
  "generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh" \
  "bash '$ROOT_DIR/scripts/generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh' --policy-id path_batch_adaptive --convergence-report docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md --output '$REL_BASE/adaptive_policy.md'" \
  "$REL_BASE/adaptive_policy.md" \
  "| adaptive_status |"

run_path_case \
  "generate_gate_archive_evidence_template_draft.sh" \
  "bash '$ROOT_DIR/scripts/generate_gate_archive_evidence_template_draft.sh' --run-id path_batch_gate_template --output '$REL_BASE/gate_template.md'" \
  "$REL_BASE/gate_template.md" \
  "| report_id |"

run_path_case \
  "manage_archive_audit_writeback_payload_versioning_rollback_draft.sh" \
  "bash '$ROOT_DIR/scripts/manage_archive_audit_writeback_payload_versioning_rollback_draft.sh' --version-id path_batch_versioning --writeback-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md --output '$REL_BASE/versioning_rollback.md'" \
  "$REL_BASE/versioning_rollback.md" \
  "| versioning_status |"

run_path_case \
  "retry_closure_acceptance_failure_draft.sh" \
  "bash '$ROOT_DIR/scripts/retry_closure_acceptance_failure_draft.sh' --retry-id path_batch_retry --closure-gate-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md --autofix-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md --verify-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_VERIFY_SAMPLE_B55.md --output '$REL_BASE/retry.md'" \
  "$REL_BASE/retry.md" \
  "| retry_status |"

run_path_case \
  "revalidate_closure_gate_after_autofix_draft.sh" \
  "bash '$ROOT_DIR/scripts/revalidate_closure_gate_after_autofix_draft.sh' --revalidate-id path_batch_revalidate --autofix-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md --closure-gate-script scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh --closure-gate-args '--tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md --sla-rollback-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md --gate-id path_batch_revalidate_gate' --output '$REL_BASE/revalidate.md'" \
  "$REL_BASE/revalidate.md" \
  "| revalidation_status |"

run_path_case \
  "review_closure_gate_weekly_trend_drift_draft.sh" \
  "bash '$ROOT_DIR/scripts/review_closure_gate_weekly_trend_drift_draft.sh' --review-id path_batch_review --gate-report-glob 'docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md' --output '$REL_BASE/review_trend.md'" \
  "$REL_BASE/review_trend.md" \
  "| review_status |"

run_path_case \
  "run_archive_audit_blocker_retest_regression_gate_draft.sh" \
  "bash '$ROOT_DIR/scripts/run_archive_audit_blocker_retest_regression_gate_draft.sh' --gate-id path_batch_retest --closure-record docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md --output '$REL_BASE/retest_regression_gate.md'" \
  "$REL_BASE/retest_regression_gate.md" \
  "| regression_gate_status |"

run_path_case \
  "track_archive_audit_writeback_change_coverage_remediation_draft.sh" \
  "bash '$ROOT_DIR/scripts/track_archive_audit_writeback_change_coverage_remediation_draft.sh' --tracker-id path_batch_tracker --writeback-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md --adaptive-policy docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md --output '$REL_BASE/tracker.md'" \
  "$REL_BASE/tracker.md" \
  "| tracker_status |"

run_path_case \
  "triage_archive_audit_evidence_anomaly_grading_response_draft.sh" \
  "bash '$ROOT_DIR/scripts/triage_archive_audit_evidence_anomaly_grading_response_draft.sh' --response-id path_batch_triage --audit-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_SAMPLE_B43.md --adaptive-policy docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md --output '$REL_BASE/triage.md'" \
  "$REL_BASE/triage.md" \
  "| response_status |"

run_path_case \
  "validate_archive_audit_blocker_closure_waiver_draft.sh" \
  "bash '$ROOT_DIR/scripts/validate_archive_audit_blocker_closure_waiver_draft.sh' --record-id path_batch_closure_waiver --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md --output '$REL_BASE/closure_waiver.md'" \
  "$REL_BASE/closure_waiver.md" \
  "| closure_status |"

run_path_case \
  "validate_archive_audit_retest_approval_writeback_linkage_draft.sh" \
  "bash '$ROOT_DIR/scripts/validate_archive_audit_retest_approval_writeback_linkage_draft.sh' --linkage-id path_batch_linkage --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md --writeback docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md --output '$REL_BASE/linkage.md'" \
  "$REL_BASE/linkage.md" \
  "| linkage_status |"

run_path_case \
  "validate_archive_audit_writeback_coverage_closure_gate_draft.sh" \
  "bash '$ROOT_DIR/scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh' --gate-id path_batch_acceptance --tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md --sla-rollback-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md --output '$REL_BASE/acceptance_gate.md'" \
  "$REL_BASE/acceptance_gate.md" \
  "| acceptance_status |"

run_path_case \
  "writeback_archive_audit_execution_receipt_after_approval_draft.sh" \
  "bash '$ROOT_DIR/scripts/writeback_archive_audit_execution_receipt_after_approval_draft.sh' --writeback-id path_batch_writeback --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md --output '$REL_BASE/execution_receipt_writeback.md'" \
  "$REL_BASE/execution_receipt_writeback.md" \
  "| writeback_status |"

echo "[PASS] batch path contracts passed"
