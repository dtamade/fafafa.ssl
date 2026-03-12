#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

compile() {
  local source_file="$1"
  local output_file="$2"
  if ! fpc -Fu./src "$source_file" -o"$output_file" >/tmp/sha3-compat-compile.log 2>&1; then
    echo "[INFO] compile output for $source_file:"
    sed -n '1,220p' /tmp/sha3-compat-compile.log || true
    fail "compile failed for $source_file"
  fi
}

compile examples/test_sha3.lpr tmp/test_sha3_example_contract
compile examples/test_openssl_sha3.lpr tmp/test_openssl_sha3_contract
compile tests/examples/test_sha3_diagnostic.pas tmp/test_sha3_diag_contract

echo '[PASS] SHA3 compatibility entrypoints compile'
