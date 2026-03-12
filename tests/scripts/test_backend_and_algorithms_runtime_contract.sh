#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

compile_and_run() {
  local source_file="$1"
  local output_file="$2"
  local pass_token="$3"
  local log_file="$4"

  if ! fpc -Fu./src "$source_file" -o"$output_file" >/tmp/backend-algo-compile.log 2>&1; then
    echo '[INFO] compile output:'
    sed -n '1,260p' /tmp/backend-algo-compile.log || true
    fail "$source_file should compile"
  fi

  if ! "./$output_file" >"$log_file" 2>&1; then
    echo '[INFO] runtime output:'
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should run successfully"
  fi

  if ! rg -F --quiet -- "$pass_token" "$log_file"; then
    echo '[INFO] runtime output:'
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should print success marker"
  fi
}

compile_and_run tests/test_backend_capabilities.pas tmp/test_backend_capabilities_contract '[PASS] backend capabilities validation completed' /tmp/test_backend_capabilities_contract.log
compile_and_run tests/test_algorithms_batch.pas tmp/test_algorithms_batch_contract '[PASS] algorithms batch validation completed' /tmp/test_algorithms_batch_contract.log

echo '[PASS] backend capability and algorithm batch programs stay green at runtime'
