#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_b101_validation_playbook.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave_c b101 validation playbook isolation passthrough contract"

if ! rg -F --quiet -- "python3 scripts/compile_all_modules.py --unit-output-dir '\$COMPILE_UNIT_OUTPUT_DIR'" "$SCRIPT"; then
  fail "compile step should passthrough --unit-output-dir"
fi

if ! rg -F --quiet -- "FAFAFA_FPC_UNIT_OUTPUT_DIR='\$MODULE_UNIT_OUTPUT_DIR'" "$SCRIPT"; then
  fail "module step should passthrough FAFAFA_FPC_UNIT_OUTPUT_DIR"
fi

if ! rg -F --quiet -- "FAFAFA_TEST_BIN_DIR='\$MODULE_BIN_OUTPUT_DIR'" "$SCRIPT"; then
  fail "module step should passthrough FAFAFA_TEST_BIN_DIR"
fi

if ! rg -F --quiet -- 'REPORT_DIR="${FAFAFA_WAVE_C_B101_REPORT_DIR:-tmp/wave_c_b101_reports_${RUN_ID}}"' "$SCRIPT"; then
  fail "script should define isolated tmp report dir default"
fi

if ! rg -F --quiet -- 'OUTPUT_FILE="$REPORT_DIR/wave_c_b101_validation_${RUN_ID}.md"' "$SCRIPT"; then
  fail "default report output should live under REPORT_DIR"
fi

if ! rg -F --quiet -- 'COMPILE_LOG="$REPORT_DIR/wave_c_b101_compile_${RUN_ID}.log"' "$SCRIPT"; then
  fail "compile log should live under REPORT_DIR"
fi

echo "[PASS] wave_c b101 validation playbook isolation passthrough contract passed"
