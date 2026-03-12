#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate compile/module isolation passthrough contract"

OUT="$(
  cd /tmp
  FAFAFA_MINIMAL_CI_GATE_RUN_ID="contract_min_gate" \
  FAFAFA_MINIMAL_CI_GATE_COMPILE_UNIT_OUTPUT_DIR="tmp/contract_compile_units" \
  FAFAFA_MINIMAL_CI_GATE_MODULE_UNIT_OUTPUT_DIR="tmp/contract_module_units" \
  FAFAFA_MINIMAL_CI_GATE_MODULE_BIN_OUTPUT_DIR="tmp/contract_module_bin" \
  bash "$SCRIPT" --dry-run --modules PKCS7 --skip-phase2-dryrun --skip-platform-path-checks-dryrun 2>&1
)"

if [[ "$OUT" != *"python3 scripts/compile_all_modules.py --unit-output-dir 'tmp/contract_compile_units'"* ]]; then
  echo "$OUT"
  fail "compile step should pass isolated --unit-output-dir"
fi

if [[ "$OUT" != *"FAFAFA_FPC_UNIT_OUTPUT_DIR='tmp/contract_module_units'"* ]]; then
  echo "$OUT"
  fail "module step should pass isolated FAFAFA_FPC_UNIT_OUTPUT_DIR"
fi

if [[ "$OUT" != *"FAFAFA_TEST_BIN_DIR='tmp/contract_module_bin'"* ]]; then
  echo "$OUT"
  fail "module step should pass isolated FAFAFA_TEST_BIN_DIR"
fi

if [[ "$OUT" != *"bash scripts/run_all_module_tests.sh --modules PKCS7"* ]]; then
  echo "$OUT"
  fail "module step should still invoke run_all_module_tests with selected modules"
fi

echo "[PASS] minimal ci gate compile/module isolation passthrough contract passed"
