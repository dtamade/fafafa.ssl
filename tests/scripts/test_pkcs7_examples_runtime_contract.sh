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

compile_and_run examples/pkcs7_basic_example.pas tmp/pkcs7_basic_example_contract '[PASS] pkcs7 basic example completed' /tmp/pkcs7_basic_example_contract.log
compile_and_run examples/pkcs7_data_example.pas tmp/pkcs7_data_example_contract '[PASS] pkcs7 data example completed' /tmp/pkcs7_data_example_contract.log
compile_and_run examples/pkcs7_encrypt_decrypt_example.pas tmp/pkcs7_encrypt_decrypt_example_contract '[PASS] pkcs7 encrypt/decrypt example completed' /tmp/pkcs7_encrypt_decrypt_example_contract.log
compile_and_run examples/pkcs7_sign_verify_example.pas tmp/pkcs7_sign_verify_example_contract '[PASS] pkcs7 sign/verify example completed' /tmp/pkcs7_sign_verify_example_contract.log

echo '[PASS] pkcs7 example programs stay green at runtime'
