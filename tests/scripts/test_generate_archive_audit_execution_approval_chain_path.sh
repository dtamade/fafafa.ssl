#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_archive_audit_execution_approval_chain_draft.sh"
REL_EXECUTION="docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md"
REL_CLOSURE="docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md"
REL_REMEDIATION="docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_SAMPLE_B37.md"
REL_BACKTEST="docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md"
REL_OUTPUT="tmp/test_archive_execution_approval_chain/path_contract.md"

require_line() {
  local file="$1"
  local expected="$2"
  if ! rg -F --quiet "$expected" "$file"; then
    echo "[FAIL] missing expected line: $expected"
    echo "[INFO] top of output:"
    sed -n '1,140p' "$file"
    exit 1
  fi
}

run_main_contract() {
  local chain_id="tdd_b39_path_contract"
  local out="$ROOT_DIR/$REL_OUTPUT"
  mkdir -p "$(dirname "$out")"
  rm -f "$out"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --chain-id "$chain_id" \
    --execution-receipt "$REL_EXECUTION" \
    --closure-record "$REL_CLOSURE" \
    --remediation-plan "$REL_REMEDIATION" \
    --backtest-report "$REL_BACKTEST" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  require_line "$out" "| chain_id | $chain_id |"
  require_line "$out" "| approval_status | fail |"

  rm -f "$out"

  (cd /tmp && bash "$SCRIPT" \
    --chain-id "$chain_id" \
    --execution-receipt "$REL_EXECUTION" \
    --closure-record "$REL_CLOSURE" \
    --remediation-plan "$REL_REMEDIATION" \
    --backtest-report "$REL_BACKTEST" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  require_line "$out" "| chain_id | $chain_id |"
  echo "[PASS] path resolution contract passed"
}

run_strict_contract() {
  if bash "$SCRIPT" \
    --chain-id strict_case_chain \
    --execution-receipt "$ROOT_DIR/$REL_EXECUTION" \
    --closure-record "$ROOT_DIR/$REL_CLOSURE" \
    --remediation-plan "$ROOT_DIR/$REL_REMEDIATION" \
    --backtest-report "$ROOT_DIR/$REL_BACKTEST" \
    --output "$ROOT_DIR/tmp/test_archive_execution_approval_chain/strict.md" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass approval status"
    exit 1
  fi

  echo "[PASS] strict mode contract passed"
}

if [[ "${1:-}" == "--strict-check" ]]; then
  run_strict_contract
  exit 0
fi

run_main_contract
