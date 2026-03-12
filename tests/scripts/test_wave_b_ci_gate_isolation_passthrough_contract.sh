#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_ci_gate.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave_b ci gate isolation passthrough contract"

OUT="$(
  cd /tmp
  FAFAFA_WAVE_B_CI_GATE_RUN_ID="contract_wave_b_ci" \
  FAFAFA_WAVE_B_CI_GATE_COMPILE_UNIT_OUTPUT_DIR="tmp/contract_wave_b_ci_compile_units" \
  FAFAFA_WAVE_B_CI_GATE_MODULE_UNIT_OUTPUT_DIR="tmp/contract_wave_b_ci_module_units" \
  FAFAFA_WAVE_B_CI_GATE_MODULE_BIN_OUTPUT_DIR="tmp/contract_wave_b_ci_module_bin" \
  bash "$SCRIPT" --dry-run --skip-examples --modules PKCS7 2>&1
)"

if [[ "$OUT" != *"python3 scripts/compile_all_modules.py --unit-output-dir 'tmp/contract_wave_b_ci_compile_units'"* ]]; then
  echo "$OUT"
  fail "compile step should passthrough --unit-output-dir"
fi

if [[ "$OUT" != *"FAFAFA_FPC_UNIT_OUTPUT_DIR='tmp/contract_wave_b_ci_module_units'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_FPC_UNIT_OUTPUT_DIR"
fi

if [[ "$OUT" != *"FAFAFA_TEST_BIN_DIR='tmp/contract_wave_b_ci_module_bin'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_TEST_BIN_DIR"
fi

echo "[PASS] wave_b ci gate isolation passthrough contract passed"
