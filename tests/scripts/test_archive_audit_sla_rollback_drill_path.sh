#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/drill_archive_audit_sla_rollback_linkage_draft.sh"
REL_SLA_REPORT="docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md"
REL_ROLLBACK_REPORT="docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md"
REL_DRILL_PLAN_REPORT="docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md"
REL_OUTPUT="tmp/test_archive_sla_rollback_drill/path_contract.md"

require_line() {
  local file="$1"
  local expected="$2"
  if ! rg -F --quiet "$expected" "$file"; then
    echo "[FAIL] missing expected line: $expected"
    echo "[INFO] top of output:"
    sed -n '1,120p' "$file"
    exit 1
  fi
}

run_main_contract() {
  local exercise_id="tdd_b52_path_contract"
  local out="$ROOT_DIR/$REL_OUTPUT"
  mkdir -p "$(dirname "$out")"
  rm -f "$out"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --exercise-id "$exercise_id" \
    --sla-alert-report "$REL_SLA_REPORT" \
    --rollback-report "$REL_ROLLBACK_REPORT" \
    --drill-plan-report "$REL_DRILL_PLAN_REPORT" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  require_line "$out" "| exercise_id | $exercise_id |"

  rm -f "$out"
  (cd /tmp && bash "$SCRIPT" \
    --exercise-id "$exercise_id" \
    --sla-alert-report "$REL_SLA_REPORT" \
    --rollback-report "$REL_ROLLBACK_REPORT" \
    --drill-plan-report "$REL_DRILL_PLAN_REPORT" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  require_line "$out" "| exercise_id | $exercise_id |"
  echo "[PASS] path resolution contract passed"
}

run_strict_contract() {
  local work="$ROOT_DIR/tmp/test_archive_sla_rollback_drill"
  local sla="$work/sla_min.md"
  local rollback="$work/rollback_min.md"
  local drill="$work/drill_min.md"
  local out="$work/strict_result.md"
  mkdir -p "$work"

  cat > "$sla" <<'MD'
# SLA

| metric | value |
|--------|-------|
| sla_breach_status | pass |
| total_alert_items | 0 |
| critical_alert_items | 0 |
| high_alert_items | 0 |

## 4) Alert Rows

| alert_id | source | owner | target_sla | target_minutes | observed | alert_level | escalation_action |
|----------|--------|-------|------------|----------------|----------|-------------|-------------------|
MD

  cat > "$rollback" <<'MD'
# Rollback

| metric | value |
|--------|-------|
| versioning_status | pass |
| rollback_candidates | 0 |
| target_version | v1.0.0 |
| rollback_version | v0.9.9 |

## 5) Rollback Queue

| blocker_code | current_status | rollback_version | rollback_reason | note |
|--------------|----------------|------------------|-----------------|------|
MD

  cat > "$drill" <<'MD'
# Drill

## 5) Rollback Exercise Queue

| queue_id | owner | action | precheck | rollback_action | verify_action | status |
|----------|-------|--------|----------|-----------------|---------------|--------|
MD

  if bash "$SCRIPT" \
    --exercise-id strict_contract_case \
    --sla-alert-report "$sla" \
    --rollback-report "$rollback" \
    --drill-plan-report "$drill" \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass linkage status"
    exit 1
  fi

  echo "[PASS] strict mode contract passed"
}

if [[ "${1:-}" == "--strict-check" ]]; then
  run_strict_contract
  exit 0
fi

run_main_contract
