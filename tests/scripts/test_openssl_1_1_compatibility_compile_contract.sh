#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

if ! fpc -Fu./src tests/openssl/test_openssl_1_1_compatibility.pas -otmp/test_openssl_1_1_compatibility_contract >/tmp/test_openssl_1_1_compatibility_contract.log 2>&1; then
  echo '[INFO] compile output:'
  sed -n '1,260p' /tmp/test_openssl_1_1_compatibility_contract.log || true
  fail 'test_openssl_1_1_compatibility should compile'
fi

echo '[PASS] test_openssl_1_1_compatibility compiles'
