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

  if ! fpc -Fu./src -Fu./tests/framework "$source_file" -o"$output_file" >/tmp/$(basename "$output_file").compile.log 2>&1; then
    echo "[INFO] compile output for $source_file:"
    sed -n '1,220p' /tmp/$(basename "$output_file").compile.log || true
    fail "$source_file should compile with framework path"
  fi

  if ! "./$output_file" >"$log_file" 2>&1; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should run successfully"
  fi

  if ! rg -F --quiet -- "$pass_token" "$log_file"; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should print completion marker"
  fi
}

compile_and_run tests/integration/test_bn_simple.pas tmp/test_bn_simple_contract '[PASS] bn simple integration completed' /tmp/test_bn_simple_contract.log
compile_and_run tests/integration/test_asn1_simple.pas tmp/test_asn1_simple_contract '[PASS] asn1 simple integration completed' /tmp/test_asn1_simple_contract.log
compile_and_run tests/integration/test_bio_simple.pas tmp/test_bio_simple_contract '[PASS] bio simple integration completed' /tmp/test_bio_simple_contract.log
compile_and_run tests/integration/test_e2e_scenarios.pas tmp/test_e2e_scenarios_contract '[PASS] e2e scenarios integration completed' /tmp/test_e2e_scenarios_contract.log
compile_and_run tests/integration/test_integration_tls_end_to_end.pas tmp/test_integration_tls_end_to_end_contract '[PASS] tls end-to-end integration completed' /tmp/test_integration_tls_end_to_end_contract.log

echo '[PASS] integration runtime smoke programs stay green'
