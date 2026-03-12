#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/continuous_test_monitor.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local pattern="$1"
  if ! rg -F --quiet -- "$pattern" "$SCRIPT"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,260p' "$SCRIPT" || true
    exit 1
  fi
}

echo "[TEST] continuous test monitor output governance contract"

assert_contains 'MONITOR_DIR="$REPORTS_DIR/monitor"'
assert_contains 'HISTORY_FILE="$MONITOR_DIR/test_history.csv"'
assert_contains 'SUMMARY_FILE="$MONITOR_DIR/monitor_summary.txt"'
assert_contains 'TREND_FILE="$MONITOR_DIR/trend_report.txt"'
assert_contains 'RUNS_DIR="$REPORTS_DIR/runs"'
assert_contains 'unit_output_dir="$RUNS_DIR/continuous_monitor_units_${run_id}"'
assert_contains 'bin_output_dir="$RUNS_DIR/continuous_monitor_bin_${run_id}"'

echo "[PASS] continuous test monitor output governance contract passed"
