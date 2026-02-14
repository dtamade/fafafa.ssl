#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/backtest_archive_audit_threshold_policy_draft.sh"

WORK_REL="tmp/test_archive_audit_threshold_policy_backtest_contract"
WORK="$ROOT_DIR/$WORK_REL"
REL_OUTPUT="$WORK_REL/backtest.md"

REL_DASHBOARD_GLOB="$WORK_REL/dashboard_*.md"

write_fixtures() {
  mkdir -p "$WORK"

  cat > "$WORK/dashboard_1.md" <<'MD'
# Archive Audit Status Dashboard

| metric | value |
|--------|-------|
| hold_overdue_total | 0 |
| hold_due_soon_total | 0 |
| hold_missing_or_invalid_expiry_total | 0 |
| checklist_readiness_fail | 0 |
| checklist_readiness_warn_or_unknown | 0 |
| weekly_fail_count | 0 |
| blocking_reason_total | 0 |
| linkage_risk_total | 0 |
MD

  cat > "$WORK/dashboard_2.md" <<'MD'
# Archive Audit Status Dashboard

| metric | value |
|--------|-------|
| hold_overdue_total | 1 |
| hold_due_soon_total | 0 |
| hold_missing_or_invalid_expiry_total | 0 |
| checklist_readiness_fail | 0 |
| checklist_readiness_warn_or_unknown | 0 |
| weekly_fail_count | 0 |
| blocking_reason_total | 0 |
| linkage_risk_total | 0 |
MD
}

assert_backtest_status_fail() {
  local report="$1"
  if ! grep -qE "^\\| backtest_status \\| fail \\|" "$report"; then
    echo "[FAIL] expected backtest_status=fail in report: $report"
    exit 1
  fi
}

run_path_contract() {
  write_fixtures
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --backtest-id path_contract_root \
    --dashboard-glob "$REL_DASHBOARD_GLOB" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  assert_backtest_status_fail "$ROOT_DIR/$REL_OUTPUT"

  rm -f "$ROOT_DIR/$REL_OUTPUT"
  rm -f "/tmp/$REL_OUTPUT" 2>/dev/null || true

  # Key contract: should still resolve relative --output under project root.
  (cd /tmp && bash "$SCRIPT" \
    --backtest-id path_contract_tmp \
    --dashboard-glob "$REL_DASHBOARD_GLOB" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  assert_backtest_status_fail "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] output path contract passed"
}

run_abs_glob_contract() {
  write_fixtures
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  local abs_glob="$ROOT_DIR/$REL_DASHBOARD_GLOB"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --backtest-id abs_glob_contract_root \
    --dashboard-glob "$abs_glob" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for absolute-glob contract"
    exit 1
  fi

  assert_backtest_status_fail "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] absolute glob contract passed"
}

run_strict_contract() {
  write_fixtures
  local out="$WORK/strict.md"
  rm -f "$out"

  if bash "$SCRIPT" \
    --backtest-id strict_contract_case \
    --dashboard-glob "$REL_DASHBOARD_GLOB" \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass backtest status"
    exit 1
  fi

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] strict mode should still write backtest report"
    exit 1
  fi

  assert_backtest_status_fail "$out"

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

