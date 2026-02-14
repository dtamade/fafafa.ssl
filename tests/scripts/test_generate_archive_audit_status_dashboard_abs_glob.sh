#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_archive_audit_status_dashboard_draft.sh"
ABS_HOLD="$ROOT_DIR/docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md"
ABS_LINKAGE="$ROOT_DIR/docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md"
ABS_CHECKLIST="$ROOT_DIR/docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md"
ABS_WEEKLY="$ROOT_DIR/docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md"
REL_OUTPUT="tmp/test_archive_status_dashboard_abs/path_contract.md"

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
  local dashboard_id="tdd_dashboard_abs_contract"
  local out="$ROOT_DIR/$REL_OUTPUT"
  mkdir -p "$(dirname "$out")"
  rm -f "$out"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --dashboard-id "$dashboard_id" \
    --hold-report-glob "$ABS_HOLD" \
    --linkage-report-glob "$ABS_LINKAGE" \
    --checklist-report-glob "$ABS_CHECKLIST" \
    --weekly-report-glob "$ABS_WEEKLY" \
    --output "$REL_OUTPUT" >/dev/null 2>/dev/null)

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  require_line "$out" "| dashboard_status | fail |"
  require_line "$out" "| hold_overdue_total | 1 |"
  require_line "$out" "| checklist_readiness_fail | 1 |"
  require_line "$out" "| weekly_fail_count | 1 |"

  rm -f "$out"

  (cd /tmp && bash "$SCRIPT" \
    --dashboard-id "$dashboard_id" \
    --hold-report-glob "$ABS_HOLD" \
    --linkage-report-glob "$ABS_LINKAGE" \
    --checklist-report-glob "$ABS_CHECKLIST" \
    --weekly-report-glob "$ABS_WEEKLY" \
    --output "$REL_OUTPUT" >/dev/null 2>/dev/null)

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  require_line "$out" "| dashboard_status | fail |"
  echo "[PASS] absolute-input dashboard contract passed"
}

run_strict_contract() {
  if bash "$SCRIPT" \
    --dashboard-id strict_case_dashboard \
    --hold-report-glob "$ABS_HOLD" \
    --linkage-report-glob "$ABS_LINKAGE" \
    --checklist-report-glob "$ABS_CHECKLIST" \
    --weekly-report-glob "$ABS_WEEKLY" \
    --output "$ROOT_DIR/tmp/test_archive_status_dashboard_abs/strict.md" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass dashboard"
    exit 1
  fi

  echo "[PASS] strict mode contract passed"
}

if [[ "${1:-}" == "--strict-check" ]]; then
  run_strict_contract
  exit 0
fi

run_main_contract
