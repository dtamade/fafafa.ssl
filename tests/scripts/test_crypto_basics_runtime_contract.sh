#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

BIN="tmp/test_crypto_basics_contract"
LOG="/tmp/test_crypto_basics_contract.log"

if ! fpc -Fu./src tests/crypto/test_crypto_basics.pas -o"$BIN" >/tmp/test_crypto_basics_contract.compile.log 2>&1; then
  echo '[INFO] compile output:'
  sed -n '1,260p' /tmp/test_crypto_basics_contract.compile.log || true
  fail 'test_crypto_basics should compile on the active Linux workflow'
fi

if ! "./$BIN" >"$LOG" 2>&1; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_crypto_basics should run successfully on the active Linux workflow'
fi

if ! rg -F --quiet -- '[PASS] Crypto basics smoke completed' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_crypto_basics should print its completion marker'
fi

echo '[PASS] test_crypto_basics compiles and runs on the active Linux workflow'
