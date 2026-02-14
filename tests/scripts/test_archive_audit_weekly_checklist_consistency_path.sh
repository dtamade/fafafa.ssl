#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_archive_audit_weekly_checklist_consistency_draft.sh"
REL_WEEKLY="docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md"
REL_CHECKLIST="docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md"
REL_OUTPUT="tmp/test_archive_weekly_checklist/consistency_path_contract.md"

run_main_contract() {
  mkdir -p "$ROOT_DIR/tmp/test_archive_weekly_checklist"
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --weekly-report "$REL_WEEKLY" \
    --checklist-report "$REL_CHECKLIST" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  rm -f "$ROOT_DIR/$REL_OUTPUT"

  # Key contract: should still work when invoked outside repo root.
  (cd /tmp && bash "$SCRIPT" \
    --weekly-report "$REL_WEEKLY" \
    --checklist-report "$REL_CHECKLIST" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  echo "[PASS] path resolution contract passed"
}

run_strict_contract() {
  local work="$ROOT_DIR/tmp/test_archive_weekly_checklist"
  local weekly="$work/weekly_inconsistent.md"
  local checklist="$work/checklist_inconsistent.md"
  local out="$work/strict_result.md"
  mkdir -p "$work"

  cat > "$checklist" <<'MD'
# Checklist

| metric | value |
|--------|-------|
| readiness | fail |
| blocking_reasons | overdue_signature |
| hold_overdue | 1 |
MD

  cat > "$weekly" <<'MD'
# Weekly

| metric | value |
|--------|-------|
| weekly_status | pass |
| hold_overdue_total | 0 |
| checklist_readiness_fail | 0 |
| checklist_readiness_warn | 0 |
| linkage_risk_total | 0 |
| checklist_report_inputs | 1 |

## 5) Checklist Aggregate

| checklist_source | readiness | blocking_reasons |
|------------------|-----------|------------------|
| checklist_inconsistent.md | pass | none |
MD

  if bash "$SCRIPT" --weekly-report "$weekly" --checklist-report "$checklist" --output "$out" --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on inconsistent weekly/checklist pair"
    exit 1
  fi

  echo "[PASS] strict mode contract passed"
}

if [[ "${1:-}" == "--strict-check" ]]; then
  run_strict_contract
  exit 0
fi

run_main_contract
