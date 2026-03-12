#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_linux_openssl_matrix_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] linux openssl matrix isolation passthrough contract"

OUT="$(
  cd /tmp
  FAFAFA_LINUX_MATRIX_RUN_ID="contract_linux_matrix" \
  FAFAFA_LINUX_MATRIX_COMPILE_UNIT_OUTPUT_DIR="tmp/contract_linux_matrix_compile_units" \
  FAFAFA_LINUX_MATRIX_MODULE_UNIT_OUTPUT_DIR="tmp/contract_linux_matrix_module_units" \
  FAFAFA_LINUX_MATRIX_MODULE_BIN_OUTPUT_DIR="tmp/contract_linux_matrix_module_bin" \
  bash "$SCRIPT" --dry-run --modules PKCS7 --skip-phase2-dryrun 2>&1
)"

if [[ "$OUT" != *"python3 scripts/compile_all_modules.py --unit-output-dir 'tmp/contract_linux_matrix_compile_units'"* ]]; then
  echo "$OUT"
  fail "compile step should passthrough isolated --unit-output-dir"
fi

if [[ "$OUT" != *"FAFAFA_FPC_UNIT_OUTPUT_DIR='tmp/contract_linux_matrix_module_units'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_FPC_UNIT_OUTPUT_DIR"
fi

if [[ "$OUT" != *"FAFAFA_TEST_BIN_DIR='tmp/contract_linux_matrix_module_bin'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_TEST_BIN_DIR"
fi

echo "[PASS] linux openssl matrix isolation passthrough contract passed"
