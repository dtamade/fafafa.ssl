#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] legacy openssl example compile contract"

compile_one() {
  local src="$1"
  local out="$2"
  if ! fpc -Fu"$ROOT_DIR/src" "$ROOT_DIR/$src" -o"$ROOT_DIR/$out" >/tmp/$(basename "$out").log 2>&1; then
    cat /tmp/$(basename "$out").log
    fail "$src should compile"
  fi
}

compile_one examples/test_openssl_rsa.lpr tmp/test_openssl_rsa_contract
compile_one examples/test_pem.lpr tmp/test_pem_contract

echo "[PASS] legacy openssl example compile contract passed"
