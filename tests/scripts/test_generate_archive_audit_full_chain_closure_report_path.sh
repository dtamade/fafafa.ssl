#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_archive_audit_full_chain_closure_report_draft.sh"

REL_CLOSURE_GATE="docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md"
REL_AUTOFIX="docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md"
REL_REVALIDATE="docs/test_reports/ARCHIVE_AUDIT_CLOSURE_REVALIDATE_SAMPLE_B58.md"
REL_TREND="docs/test_reports/ARCHIVE_AUDIT_CLOSURE_TREND_SAMPLE_B57.md"
REL_RETRY="docs/test_reports/ARCHIVE_AUDIT_CLOSURE_RETRY_SAMPLE_B56.md"
REL_SLA_DRILL="docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md"
REL_VERIFY="docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_VERIFY_SAMPLE_B55.md"
REL_OUTPUT="tmp/test_archive_full_chain_closure/path_contract.md"

run_main_contract() {
  mkdir -p "$ROOT_DIR/tmp/test_archive_full_chain_closure"
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --report-id path_contract_root \
    --closure-gate-report "$REL_CLOSURE_GATE" \
    --autofix-report "$REL_AUTOFIX" \
    --revalidate-report "$REL_REVALIDATE" \
    --trend-report "$REL_TREND" \
    --retry-report "$REL_RETRY" \
    --sla-drill-report "$REL_SLA_DRILL" \
    --verify-report "$REL_VERIFY" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  if ! grep -qE "^\| overall_status \| fail \|" "$ROOT_DIR/$REL_OUTPUT"; then
    echo "[FAIL] expected overall_status=fail for root-dir execution sample inputs"
    exit 1
  fi

  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd /tmp && bash "$SCRIPT" \
    --report-id path_contract_tmp \
    --closure-gate-report "$REL_CLOSURE_GATE" \
    --autofix-report "$REL_AUTOFIX" \
    --revalidate-report "$REL_REVALIDATE" \
    --trend-report "$REL_TREND" \
    --retry-report "$REL_RETRY" \
    --sla-drill-report "$REL_SLA_DRILL" \
    --verify-report "$REL_VERIFY" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  if ! grep -qE "^\| overall_status \| fail \|" "$ROOT_DIR/$REL_OUTPUT"; then
    echo "[FAIL] expected overall_status=fail for /tmp execution with relative inputs"
    exit 1
  fi

  echo "[PASS] path resolution contract passed"
}

run_strict_contract() {
  local out="$ROOT_DIR/tmp/test_archive_full_chain_closure/strict_contract.md"
  mkdir -p "$(dirname "$out")"
  rm -f "$out"

  if bash "$SCRIPT" \
    --report-id strict_contract_case \
    --closure-gate-report "$ROOT_DIR/$REL_CLOSURE_GATE" \
    --autofix-report "$ROOT_DIR/$REL_AUTOFIX" \
    --revalidate-report "$ROOT_DIR/$REL_REVALIDATE" \
    --trend-report "$ROOT_DIR/$REL_TREND" \
    --retry-report "$ROOT_DIR/$REL_RETRY" \
    --sla-drill-report "$ROOT_DIR/$REL_SLA_DRILL" \
    --verify-report "$ROOT_DIR/$REL_VERIFY" \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass full-chain status"
    exit 1
  fi

  echo "[PASS] strict mode contract passed"
}

if [[ "${1:-}" == "--strict-check" ]]; then
  run_strict_contract
  exit 0
fi

run_main_contract
