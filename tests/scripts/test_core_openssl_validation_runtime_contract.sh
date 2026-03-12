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

  if ! fpc -Fu./src "$source_file" -o"$output_file" >/tmp/core-openssl-validation-compile.log 2>&1; then
    echo "[INFO] compile output for $source_file:"
    sed -n '1,220p' /tmp/core-openssl-validation-compile.log || true
    fail "compile failed for $source_file"
  fi

  if ! "./$output_file" >"$log_file" 2>&1; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "runtime failed for $source_file"
  fi

  if ! rg -F --quiet -- "$pass_token" "$log_file"; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "runtime output missing success token for $source_file"
  fi
}

compile_and_run tests/test_core_modules_only.pas tmp/test_core_modules_only_contract '✓✓✓ 所有核心模块验证通过! ✓✓✓' /tmp/test_core_modules_only.log
compile_and_run tests/test_headers_validation.pas tmp/test_headers_validation_contract 'SUCCESS: All core module headers are valid!' /tmp/test_headers_validation.log

echo '[PASS] core OpenSSL validation programs stay green at runtime'
