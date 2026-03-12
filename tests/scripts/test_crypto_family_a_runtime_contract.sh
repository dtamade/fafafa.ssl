#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

compile_and_run() {
  local src="$1"
  local out="$2"
  local token="$3"
  local log="$4"
  if ! fpc -Fu./src "$src" -o"$out" >/tmp/$(basename "$out").log 2>&1; then
    echo "[INFO] compile output for $src:"
    sed -n '1,220p' /tmp/$(basename "$out").log || true
    fail "$src should compile"
  fi
  if ! "./$out" >"$log" 2>&1; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,220p' "$log" || true
    fail "$src should run"
  fi
  if ! rg -F --quiet -- "$token" "$log"; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,220p' "$log" || true
    fail "$src should print completion marker"
  fi
}

compile_and_run tests/crypto/test_blowfish.pas tmp/test_blowfish_contract '[PASS] blowfish smoke completed' /tmp/test_blowfish_contract.log
compile_and_run tests/crypto/test_blake2.pas tmp/test_blake2_contract '[PASS] blake2 smoke completed' /tmp/test_blake2_contract.log
compile_and_run tests/crypto/test_chacha20.pas tmp/test_chacha20_contract '[PASS] chacha20 smoke completed' /tmp/test_chacha20_contract.log
compile_and_run tests/crypto/test_camellia.pas tmp/test_camellia_contract '[PASS] camellia smoke completed' /tmp/test_camellia_contract.log
compile_and_run tests/crypto/test_sha3_simple.pas tmp/test_sha3_simple_contract '[PASS] sha3 simple smoke completed' /tmp/test_sha3_simple_contract.log
compile_and_run tests/crypto/test_ripemd.pas tmp/test_ripemd_contract '[PASS] ripemd smoke completed' /tmp/test_ripemd_contract.log
compile_and_run tests/crypto/test_sm3.pas tmp/test_sm3_contract '[PASS] sm3 smoke completed' /tmp/test_sm3_contract.log
compile_and_run tests/crypto/test_sm4.pas tmp/test_sm4_contract '[PASS] sm4 smoke completed' /tmp/test_sm4_contract.log

echo '[PASS] crypto family A programs stay green at runtime'
