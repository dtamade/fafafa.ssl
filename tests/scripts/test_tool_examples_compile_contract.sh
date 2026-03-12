#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

compile_one() {
  local src="$1"
  local out="$2"
  if ! fpc -Fu./src "$src" -o"$out" >/tmp/$(basename "$out").log 2>&1; then
    echo "[INFO] compile output for $src:"
    sed -n '1,220p' /tmp/$(basename "$out").log || true
    fail "$src should compile"
  fi
}

compile_one examples/03_file_encryption.pas tmp/file_encryption_contract
compile_one examples/file_encrypt/file_encrypt.pas tmp/file_encrypt_contract
compile_one examples/password_hash/password_hash.pas tmp/password_hash_contract
compile_one examples/hmac_tool/hmac_tool.lpr tmp/hmac_tool_contract
compile_one examples/digital_signature/digital_signature.pas tmp/digital_signature_contract

echo '[PASS] tool/example programs compile under current loader stack'
