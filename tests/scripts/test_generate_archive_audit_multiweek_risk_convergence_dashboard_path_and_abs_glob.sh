#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh"

WORK_REL="tmp/test_archive_audit_multiweek_convergence_dashboard_contract"
WORK="$ROOT_DIR/$WORK_REL"
REL_OUTPUT="$WORK_REL/dashboard.md"

REL_BACKTEST_GLOB="$WORK_REL/backtest_*.md"
REL_APPROVAL_GLOB="$WORK_REL/approval_*.md"
REL_RETEST_GLOB="$WORK_REL/retest_*.md"

write_fixtures() {
  mkdir -p "$WORK"

  cat > "$WORK/backtest_1.md" <<'MD'
# Threshold Policy Backtest

| metric | value |
|--------|-------|
| critical_runs | 0 |
| high_runs | 1 |
| backtest_status | pass |
MD

  cat > "$WORK/backtest_2.md" <<'MD'
# Threshold Policy Backtest

| metric | value |
|--------|-------|
| critical_runs | 0 |
| high_runs | 1 |
| backtest_status | fail |
MD

  cat > "$WORK/approval_1.md" <<'MD'
# Execution Approval Chain

| metric | value |
|--------|-------|
| rejected_stages | 0 |
| conditional_stages | 0 |
| approval_status | pass |
MD

  cat > "$WORK/approval_2.md" <<'MD'
# Execution Approval Chain

| metric | value |
|--------|-------|
| rejected_stages | 0 |
| conditional_stages | 0 |
| approval_status | pass |
MD

  cat > "$WORK/retest_1.md" <<'MD'
# Blocker Retest Regression Gate

| metric | value |
|--------|-------|
| retest_failed | 0 |
| open_critical_after_retest | 0 |
| regression_gate_status | pass |
MD

  cat > "$WORK/retest_2.md" <<'MD'
# Blocker Retest Regression Gate

| metric | value |
|--------|-------|
| retest_failed | 0 |
| open_critical_after_retest | 0 |
| regression_gate_status | pass |
MD
}

assert_risk_convergence_fail() {
  local report="$1"
  if ! grep -qE "^\\| risk_convergence_status \\| fail \\|" "$report"; then
    echo "[FAIL] expected risk_convergence_status=fail in report: $report"
    exit 1
  fi
}

run_path_contract() {
  write_fixtures
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --dashboard-id path_contract_root \
    --backtest-glob "$REL_BACKTEST_GLOB" \
    --approval-chain-glob "$REL_APPROVAL_GLOB" \
    --retest-gate-glob "$REL_RETEST_GLOB" \
    --trend-alert-threshold 1 \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  assert_risk_convergence_fail "$ROOT_DIR/$REL_OUTPUT"

  rm -f "$ROOT_DIR/$REL_OUTPUT"
  rm -f "/tmp/$REL_OUTPUT" 2>/dev/null || true

  # Key contract: should still resolve relative --output under project root.
  (cd /tmp && bash "$SCRIPT" \
    --dashboard-id path_contract_tmp \
    --backtest-glob "$REL_BACKTEST_GLOB" \
    --approval-chain-glob "$REL_APPROVAL_GLOB" \
    --retest-gate-glob "$REL_RETEST_GLOB" \
    --trend-alert-threshold 1 \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  assert_risk_convergence_fail "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] output path contract passed"
}

run_abs_glob_contract() {
  write_fixtures
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  local abs_backtest="$ROOT_DIR/$REL_BACKTEST_GLOB"
  local abs_approval="$ROOT_DIR/$REL_APPROVAL_GLOB"
  local abs_retest="$ROOT_DIR/$REL_RETEST_GLOB"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --dashboard-id abs_glob_contract_root \
    --backtest-glob "$abs_backtest" \
    --approval-chain-glob "$abs_approval" \
    --retest-gate-glob "$abs_retest" \
    --trend-alert-threshold 1 \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for absolute-glob contract"
    exit 1
  fi

  assert_risk_convergence_fail "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] absolute glob contract passed"
}

run_strict_contract() {
  write_fixtures
  local out="$WORK/strict.md"
  rm -f "$out"

  if bash "$SCRIPT" \
    --dashboard-id strict_contract_case \
    --backtest-glob "$REL_BACKTEST_GLOB" \
    --approval-chain-glob "$REL_APPROVAL_GLOB" \
    --retest-gate-glob "$REL_RETEST_GLOB" \
    --trend-alert-threshold 1 \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail on non-pass risk convergence status"
    exit 1
  fi

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] strict mode should still write dashboard report"
    exit 1
  fi

  assert_risk_convergence_fail "$out"

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

