#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/verify_archive_audit_sla_rollback_linkage_draft.sh"
REL_SLA_REPORT="docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md"
REL_ROLLBACK_REPORT="docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md"
REL_LINKAGE_REPORT="docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md"
REL_OUTPUT="tmp/test_archive_sla_rollback_verify/verify_report.md"

run_main_contract() {
  local verify_id="tdd_b55_path_contract"
  mkdir -p "$ROOT_DIR/tmp/test_archive_sla_rollback_verify"
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --verify-id "$verify_id" \
    --sla-alert-report "$REL_SLA_REPORT" \
    --rollback-drill-report "$REL_ROLLBACK_REPORT" \
    --linkage-drill-report "$REL_LINKAGE_REPORT" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  rm -f "$ROOT_DIR/$REL_OUTPUT"

  # Key contract: should still work when invoked outside repo root.
  (cd /tmp && bash "$SCRIPT" \
    --verify-id "$verify_id" \
    --sla-alert-report "$REL_SLA_REPORT" \
    --rollback-drill-report "$REL_ROLLBACK_REPORT" \
    --linkage-drill-report "$REL_LINKAGE_REPORT" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  if ! rg -F --quiet "| verify_id | $verify_id |" "$ROOT_DIR/$REL_OUTPUT"; then
    echo "[FAIL] expected verify_id not found in generated report"
    exit 1
  fi

  echo "[PASS] path resolution contract passed"
}

run_strict_contract() {
  local work="$ROOT_DIR/tmp/test_archive_sla_rollback_verify"
  local empty_sla="$work/empty_sla.md"
  local out="$work/strict_result.md"
  mkdir -p "$work"
  : > "$empty_sla"

  if bash "$SCRIPT" \
    --verify-id strict_contract_case \
    --sla-alert-report "$empty_sla" \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail when verification has fail checks"
    exit 1
  fi

  echo "[PASS] strict mode contract passed"
}

if [[ "${1:-}" == "--strict-check" ]]; then
  run_strict_contract
  exit 0
fi

run_main_contract
