#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_archive_audit_weekly_report_draft.sh"

WORK_REL="tmp/test_archive_weekly_report_contract"
WORK="$ROOT_DIR/$WORK_REL"
REL_OUTPUT="$WORK_REL/weekly_report.md"

REL_HOLD="$WORK_REL/hold.md"
REL_LINKAGE="$WORK_REL/linkage.md"
REL_CHECKLIST="$WORK_REL/checklist.md"

write_fixtures() {
  mkdir -p "$WORK"

  cat > "$WORK/hold.md" <<'MD'
# Hold Expiry Review

| metric | value |
|--------|-------|
| overdue | 1 |
| due_soon | 0 |
| missing_expiry | 0 |
| invalid_expiry | 0 |
MD

  cat > "$WORK/linkage.md" <<'MD'
# Hold Linkage

| metric | value |
|--------|-------|
| sampled_runs_risk | 0 |
| status | pass |
MD

  cat > "$WORK/checklist.md" <<'MD'
# Pre-Release Checklist

| metric | value |
|--------|-------|
| readiness | pass |
| blocking_reasons | none |
MD
}

assert_weekly_status_fail() {
  local report="$1"
  if ! grep -qE "^\\| weekly_status \\| fail \\|" "$report"; then
    echo "[FAIL] expected weekly_status=fail in report: $report"
    exit 1
  fi
}

run_path_contract() {
  write_fixtures
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --week-id path_contract_root \
    --hold-report-glob "$REL_HOLD" \
    --linkage-report-glob "$REL_LINKAGE" \
    --checklist-report-glob "$REL_CHECKLIST" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  assert_weekly_status_fail "$ROOT_DIR/$REL_OUTPUT"

  rm -f "$ROOT_DIR/$REL_OUTPUT"

  # Key contract: should still resolve relative --output under project root.
  (cd /tmp && bash "$SCRIPT" \
    --week-id path_contract_tmp \
    --hold-report-glob "$REL_HOLD" \
    --linkage-report-glob "$REL_LINKAGE" \
    --checklist-report-glob "$REL_CHECKLIST" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  assert_weekly_status_fail "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] output path contract passed"
}

run_abs_glob_contract() {
  write_fixtures
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  local abs_hold="$ROOT_DIR/$REL_HOLD"
  local abs_linkage="$ROOT_DIR/$REL_LINKAGE"
  local abs_checklist="$ROOT_DIR/$REL_CHECKLIST"

  # Absolute paths passed via *glob* options must be handled correctly.
  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --week-id abs_glob_contract_root \
    --hold-report-glob "$abs_hold" \
    --linkage-report-glob "$abs_linkage" \
    --checklist-report-glob "$abs_checklist" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for absolute-glob contract"
    exit 1
  fi

  assert_weekly_status_fail "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] absolute glob contract passed"
}

run_strict_contract() {
  write_fixtures
  local out="$WORK/strict.md"
  rm -f "$out"

  if bash "$SCRIPT" \
    --week-id strict_contract_case \
    --hold-report-glob "$REL_HOLD" \
    --linkage-report-glob "$REL_LINKAGE" \
    --checklist-report-glob "$REL_CHECKLIST" \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass weekly status"
    exit 1
  fi

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] strict mode should still write weekly report"
    exit 1
  fi

  assert_weekly_status_fail "$out"

  echo "[PASS] strict mode contract passed"
}

case "${1:-}" in
  --abs-check)
    run_abs_glob_contract
    ;;
  --strict-check)
    run_strict_contract
    ;;
  *)
    run_path_contract
    ;;
esac

