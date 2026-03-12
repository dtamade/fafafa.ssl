#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/continuous_test_monitor.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] continuous test monitor isolation passthrough contract"

if ! rg -F --quiet -- 'FAFAFA_FPC_UNIT_OUTPUT_DIR="$unit_output_dir"' "$SCRIPT"; then
  fail "monitor should passthrough FAFAFA_FPC_UNIT_OUTPUT_DIR per run"
fi

if ! rg -F --quiet -- 'FAFAFA_TEST_BIN_DIR="$bin_output_dir"' "$SCRIPT"; then
  fail "monitor should passthrough FAFAFA_TEST_BIN_DIR per run"
fi

if ! rg -F --quiet -- '"$SCRIPTS_DIR/run_all_module_tests.sh" "${test_args[@]}"' "$SCRIPT"; then
  fail "monitor should invoke run_all_module_tests with array-safe args"
fi

echo "[PASS] continuous test monitor isolation passthrough contract passed"
