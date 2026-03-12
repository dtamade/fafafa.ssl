#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
QUICK_WORKFLOW="$ROOT_DIR/.github/workflows/wave-c-quick-sprint-manual.yml.disabled"
MATRIX_WORKFLOW="$ROOT_DIR/.github/workflows/ci-matrix-draft.yml"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,340p' "$file" || true
    fail "expected pattern not found"
  fi
}

assert_not_contains() {
  local file="$1"
  local pattern="$2"
  if rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] unexpected pattern '$pattern' in $file"
    sed -n '1,340p' "$file" || true
    fail "unexpected pattern found"
  fi
}

echo "[TEST] repo hygiene workflow wave c quick and ci matrix tmp report paths contract"

assert_contains "$QUICK_WORKFLOW" '--output "tmp/wave_c_quick_sprint_reports/wave_c_b101_validation_${RUN_ID}.md"'
assert_contains "$QUICK_WORKFLOW" '--output "tmp/wave_c_quick_sprint_reports/wave_c_quick_sprint_bundle_${RUN_ID}.md"'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b101_validation_${{ steps.runid.outputs.run_id }}.md'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b107_threshold_eval_${{ steps.runid.outputs.run_id }}.md'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b108_default_on_readiness_${{ steps.runid.outputs.run_id }}.md'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b109_canary_rollout_${{ steps.runid.outputs.run_id }}.md'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b110_rollback_drill_${{ steps.runid.outputs.run_id }}.md'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_quick_sprint_bundle_${{ steps.runid.outputs.run_id }}.md'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b107_threshold_eval_${{ steps.runid.outputs.run_id }}.log'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b108_default_on_readiness_${{ steps.runid.outputs.run_id }}.log'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b109_canary_rollout_${{ steps.runid.outputs.run_id }}.log'
assert_contains "$QUICK_WORKFLOW" 'tmp/wave_c_quick_sprint_reports/wave_c_b110_rollback_drill_${{ steps.runid.outputs.run_id }}.log'
assert_not_contains "$QUICK_WORKFLOW" 'test-reports/'

assert_contains "$MATRIX_WORKFLOW" 'REPORTS_DIR="tmp/ci_matrix_draft_reports/linux-${{ matrix.openssl }}"'
assert_contains "$MATRIX_WORKFLOW" 'FAFAFA_TEST_REPORTS_DIR="$REPORTS_DIR" ./scripts/run_all_module_tests.sh --verbose || true'
assert_contains "$MATRIX_WORKFLOW" 'path: tmp/ci_matrix_draft_reports/linux-${{ matrix.openssl }}/'
assert_contains "$MATRIX_WORKFLOW" 'REPORTS_DIR="tmp/ci_matrix_draft_reports/macos"'
assert_contains "$MATRIX_WORKFLOW" 'fpc -Mobjfpc -Sh -Fu./src examples/hello_ssl.pas -o./bin/hello_ssl > "$REPORTS_DIR/hello_ssl_build.log" 2>&1 || true'
assert_contains "$MATRIX_WORKFLOW" './bin/hello_ssl > "$REPORTS_DIR/hello_ssl_run.log" 2>&1 || true'
assert_contains "$MATRIX_WORKFLOW" 'tmp/ci_matrix_draft_reports/macos/'
assert_contains "$MATRIX_WORKFLOW" 'REPORTS_DIR=tmp\ci_matrix_draft_reports\windows'
assert_contains "$MATRIX_WORKFLOW" 'fpc -Mobjfpc -Sh -Fu.\src examples\hello_ssl.pas -o.\bin\hello_ssl.exe > %REPORTS_DIR%\hello_ssl_build.log 2>&1'
assert_contains "$MATRIX_WORKFLOW" '.\bin\hello_ssl.exe > %REPORTS_DIR%\hello_ssl_run.log 2>&1'
assert_contains "$MATRIX_WORKFLOW" 'tmp/ci_matrix_draft_reports/windows/'
assert_not_contains "$MATRIX_WORKFLOW" 'test-reports/'

echo "[PASS] repo hygiene workflow wave c quick and ci matrix tmp report paths contract passed"
