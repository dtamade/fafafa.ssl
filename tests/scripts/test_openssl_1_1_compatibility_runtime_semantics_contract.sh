#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

BIN='tmp/test_openssl_1_1_compatibility_runtime_contract'
LOG='/tmp/test_openssl_1_1_compatibility_runtime_contract.log'

if ! fpc -Fu./src tests/openssl/test_openssl_1_1_compatibility.pas -o"$BIN" >/tmp/test_openssl_1_1_compatibility_runtime_contract.compile.log 2>&1; then
  echo '[INFO] compile output:'
  sed -n '1,260p' /tmp/test_openssl_1_1_compatibility_runtime_contract.compile.log || true
  fail 'test_openssl_1_1_compatibility should compile'
fi

set +e
"./$BIN" >"$LOG" 2>&1
STATUS=$?
set -e

if rg -F --quiet -- '[FAIL] 1.1.x Core Loading' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'missing OpenSSL 1.1.x should be reported as skip/unavailable, not as FAIL'
fi

if [[ "$STATUS" -ne 0 ]] && ! rg -F --quiet -- '[SKIP] OpenSSL 1.1.x runtime not available' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'non-zero exit is only allowed when runtime skip marker is present'
fi

echo '[PASS] OpenSSL 1.1 compatibility runtime semantics are explicit'
