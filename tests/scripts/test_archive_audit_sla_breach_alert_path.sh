#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh"

REL_APPROVAL="docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md"
REL_ANOMALY="docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md"
REL_OUTPUT="tmp/test_archive_sla_breach_alert/path_contract.md"

assert_has_status_row() {
  local report="$1"
  if ! grep -qE "^\\| sla_breach_status \\|" "$report"; then
    echo "[FAIL] expected sla_breach_status row in report: $report"
    exit 1
  fi
}

run_main_contract() {
  mkdir -p "$ROOT_DIR/tmp/test_archive_sla_breach_alert"
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --alert-id path_contract_root \
    --approval-chain-report "$REL_APPROVAL" \
    --anomaly-response "$REL_ANOMALY" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  assert_has_status_row "$ROOT_DIR/$REL_OUTPUT"

  rm -f "$ROOT_DIR/$REL_OUTPUT"

  # Key contract: should still work when invoked outside repo root.
  (cd /tmp && bash "$SCRIPT" \
    --alert-id path_contract_tmp \
    --approval-chain-report "$REL_APPROVAL" \
    --anomaly-response "$REL_ANOMALY" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  assert_has_status_row "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] path resolution contract passed"
}

run_strict_contract() {
  local work="$ROOT_DIR/tmp/test_archive_sla_breach_alert"
  local approval="$work/approval_fail.md"
  local anomaly="$work/anomaly_fail.md"
  local out="$work/strict.md"
  mkdir -p "$work"
  rm -f "$out"

  cat > "$approval" <<'MD'
# Approval Chain

| metric | value |
|--------|-------|
| approval_status | fail |
| rejected_stages | 1 |
| pending_review_stages | 0 |
| release_decision | block-release-and-escalate |

## 4) Approval Chain Rows

| stage_id | stage_name | source_report | gate_metric | gate_value | stage_status | approver_role | target_sla | approval_note | followup_action |\n+|----------|------------|---------------|------------|------------|--------------|---------------|------------|--------------|-----------------|
| S1 | gate | report.md | metric | value | fail | release-manager | <1h | n/a | open-war-room |
MD

  cat > "$anomaly" <<'MD'
# Anomaly Response

| metric | value |
|--------|-------|
| response_status | fail |
| anomalies_total | 1 |
| critical_high_open | 1 |

## 5) Response Queue

| anomaly_id | severity | owner | sla | immediate_action | status |
|------------|----------|-------|-----|------------------|--------|
| A-001 | critical | qa-secops | <1h | investigate | open |
MD

  if bash "$SCRIPT" \
    --alert-id strict_contract_case \
    --approval-chain-report "$approval" \
    --anomaly-response "$anomaly" \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass sla_breach_status"
    exit 1
  fi

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] strict mode should still write alert report"
    exit 1
  fi

  if ! grep -qE "^\\| sla_breach_status \\| fail \\|" "$out"; then
    echo "[FAIL] expected sla_breach_status=fail in strict contract output"
    exit 1
  fi

  echo "[PASS] strict mode contract passed"
}

if [[ "${1:-}" == "--strict-check" ]]; then
  run_strict_contract
  exit 0
fi

run_main_contract
