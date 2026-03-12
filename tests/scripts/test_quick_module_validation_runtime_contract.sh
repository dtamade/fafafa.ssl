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

  if ! fpc -Fu./src "$source_file" -o"$output_file" >/tmp/quick-module-validation.compile.log 2>&1; then
    echo '[INFO] compile output:'
    sed -n '1,260p' /tmp/quick-module-validation.compile.log || true
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

compile_and_run tests/test_modules_quick_validation.pas tmp/test_modules_quick_validation_contract '[PASS] quick module validation completed' /tmp/test_modules_quick_validation_contract.log
compile_and_run tests/test_priority1_modules.pas tmp/test_priority1_modules_contract '[PASS] priority1 module validation completed' /tmp/test_priority1_modules_contract.log

echo '[PASS] quick module validation programs stay green at runtime'
