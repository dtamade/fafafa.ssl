#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

WOLF_LOG="tmp/_warning_contract_wolfssl.log"
MBED_LOG="tmp/_warning_contract_mbedtls.log"

fpc -Fu./src -otmp/_warning_contract_wolfssl tests/test_wolfssl_standalone_connection_server_name_compatibility.pas > "$WOLF_LOG" 2>&1 || true
fpc -Fu./src -otmp/_warning_contract_mbedtls tests/test_mbedtls_connection_server_name_observability.pas > "$MBED_LOG" 2>&1 || true

TARGET_PATTERN='src/fafafa.ssl.(wolfssl|mbedtls).*(Warning:|Note:)|fafafa.ssl.(wolfssl|mbedtls).*(Warning:|Note:)'

if rg -n "$TARGET_PATTERN" "$WOLF_LOG" "$MBED_LOG"; then
  echo '[FAIL] focused WolfSSL/MbedTLS compile paths still emit backend-local warning/noise'
  exit 1
fi

echo '[PASS] focused WolfSSL/MbedTLS compile paths are free of backend-local warning/noise'
