#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_macos_openssl_path_check_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] macOS openssl path-check isolation passthrough contract"

OUT="$(
  cd /tmp
  FAFAFA_MACOS_PATH_CHECK_RUN_ID="contract_macos_path" \
  FAFAFA_MACOS_PATH_CHECK_MODULE_UNIT_OUTPUT_DIR="tmp/contract_macos_module_units" \
  FAFAFA_MACOS_PATH_CHECK_MODULE_BIN_OUTPUT_DIR="tmp/contract_macos_module_bin" \
  bash "$SCRIPT" \
    --dry-run \
    --skip-phase2-dryrun \
    --modules PKCS7 \
    --openssl-root /opt/homebrew/opt/openssl@3 2>&1
)"

if [[ "$OUT" != *"FAFAFA_FPC_UNIT_OUTPUT_DIR='tmp/contract_macos_module_units'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_FPC_UNIT_OUTPUT_DIR"
fi

if [[ "$OUT" != *"FAFAFA_TEST_BIN_DIR='tmp/contract_macos_module_bin'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_TEST_BIN_DIR"
fi

echo "[PASS] macOS openssl path-check isolation passthrough contract passed"
