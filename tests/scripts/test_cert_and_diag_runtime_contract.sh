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

  if ! fpc -Fu./src "$source_file" -o"$output_file" >/tmp/$(basename "$output_file").compile.log 2>&1; then
    echo "[INFO] compile output for $source_file:"
    sed -n '1,220p' /tmp/$(basename "$output_file").compile.log || true
    fail "$source_file should compile"
  fi

  if ! "./$output_file" >"$log_file" 2>&1; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should run successfully"
  fi

  if ! rg -F --quiet -- "$pass_token" "$log_file"; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should print success marker"
  fi
}

compile_and_run tests/certificate/test_bn_comprehensive.pas tmp/test_bn_comprehensive_contract '[PASS] bn comprehensive validation completed' /tmp/test_bn_comprehensive_contract.log
compile_and_run tests/certificate/test_bio_comprehensive.pas tmp/test_bio_comprehensive_contract '[PASS] bio comprehensive validation completed' /tmp/test_bio_comprehensive_contract.log
compile_and_run tests/diagnostic/diagnose_whirlpool.pas tmp/diagnose_whirlpool_contract '[PASS] diagnose whirlpool completed' /tmp/diagnose_whirlpool_contract.log
compile_and_run tests/test_phase2_simple.pas tmp/test_phase2_simple_contract '[PASS] phase2 simple validation completed' /tmp/test_phase2_simple_contract.log

echo '[PASS] cert and diagnostic runtime programs stay green'
