#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

BIN="tmp/test_openssl_load_contract"
LOG="/tmp/test_openssl_load_contract.log"

if ! fpc -Fu./src tests/openssl/test_openssl_load.pas -o"$BIN" >/tmp/test_openssl_load_contract.compile.log 2>&1; then
  echo '[INFO] compile output:'
  sed -n '1,260p' /tmp/test_openssl_load_contract.compile.log || true
  fail 'test_openssl_load should compile on the active Linux workflow'
fi

if ! "./$BIN" >"$LOG" 2>&1; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_openssl_load should run successfully on the active Linux workflow'
fi

if ! rg -F --quiet -- '[PASS] OpenSSL loader smoke completed' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_openssl_load should print its completion marker'
fi

if ! rg -F --quiet -- 'OpenSSL 版本号:' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_openssl_load should print OpenSSL version information'
fi

echo '[PASS] test_openssl_load compiles and runs on the active Linux workflow'
