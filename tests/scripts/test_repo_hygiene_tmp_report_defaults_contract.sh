#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RUN_ALL="$ROOT_DIR/scripts/run_all_module_tests.sh"
WAVE_C="$ROOT_DIR/scripts/run_wave_c_b101_validation_playbook.sh"
WAVE_B="$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,240p' "$file" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] repo hygiene tmp report defaults contract"

assert_contains "$RUN_ALL" 'DEFAULT_REPORTS_DIR="$PROJECT_ROOT/tmp/run_all_module_tests_reports_${RUN_ID}"'
assert_contains "$RUN_ALL" 'REPORTS_DIR="${FAFAFA_TEST_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'

assert_contains "$WAVE_C" '--output FILE       输出报告（默认 tmp/wave_c_b101_reports_<run_id>/wave_c_b101_validation_<run_id>.md）'
assert_contains "$WAVE_C" 'REPORT_DIR="${FAFAFA_WAVE_C_B101_REPORT_DIR:-tmp/wave_c_b101_reports_${RUN_ID}}"'
assert_contains "$WAVE_C" 'OUTPUT_FILE="$REPORT_DIR/wave_c_b101_validation_${RUN_ID}.md"'
assert_contains "$WAVE_C" 'COMPILE_LOG="$REPORT_DIR/wave_c_b101_compile_${RUN_ID}.log"'
assert_contains "$WAVE_C" 'MODULE_LOG="$REPORT_DIR/wave_c_b101_modules_${RUN_ID}.log"'
assert_contains "$WAVE_C" 'BENCH_COMPILE_LOG="$REPORT_DIR/wave_c_b101_bench_compile_${RUN_ID}.log"'
assert_contains "$WAVE_C" 'BENCH_RUN_LOG="$REPORT_DIR/wave_c_b101_bench_run_${RUN_ID}.log"'

assert_contains "$WAVE_B" '--output FILE             输出文件（默认 tmp/wave_b_reports/wave_b_cross_platform_summary_<run_id>.md）'
assert_contains "$WAVE_B" 'REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$WAVE_B" 'OUTPUT_FILE="$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"'

echo "[PASS] repo hygiene tmp report defaults contract passed"
