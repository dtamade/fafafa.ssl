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

PASS_TOKEN='RESULT: ALL TESTS PASSED'

compile_and_run tests/integration/test_asn1_simple.pas tmp/test_asn1_simple_runtime_contract "$PASS_TOKEN" /tmp/test_asn1_simple_runtime_contract.log
compile_and_run tests/integration/test_bn_simple.pas tmp/test_bn_simple_runtime_contract "$PASS_TOKEN" /tmp/test_bn_simple_runtime_contract.log
compile_and_run tests/integration/test_bio_simple.pas tmp/test_bio_simple_runtime_contract "$PASS_TOKEN" /tmp/test_bio_simple_runtime_contract.log
compile_and_run tests/integration/test_hmac_simple.pas tmp/test_hmac_simple_runtime_contract "$PASS_TOKEN" /tmp/test_hmac_simple_runtime_contract.log
compile_and_run tests/integration/test_rand_simple.pas tmp/test_rand_simple_runtime_contract "$PASS_TOKEN" /tmp/test_rand_simple_runtime_contract.log
compile_and_run tests/integration/test_rsa_simple.pas tmp/test_rsa_simple_runtime_contract "$PASS_TOKEN" /tmp/test_rsa_simple_runtime_contract.log
compile_and_run tests/integration/test_x509_simple.pas tmp/test_x509_simple_runtime_contract "$PASS_TOKEN" /tmp/test_x509_simple_runtime_contract.log
compile_and_run tests/integration/test_x509_basic.pas tmp/test_x509_basic_runtime_contract "$PASS_TOKEN" /tmp/test_x509_basic_runtime_contract.log
compile_and_run tests/integration/test_buffer_simple.pas tmp/test_buffer_simple_runtime_contract "$PASS_TOKEN" /tmp/test_buffer_simple_runtime_contract.log
compile_and_run tests/integration/test_dsa_simple.pas tmp/test_dsa_simple_runtime_contract "$PASS_TOKEN" /tmp/test_dsa_simple_runtime_contract.log
compile_and_run tests/integration/test_ec_simple.pas tmp/test_ec_simple_runtime_contract "$PASS_TOKEN" /tmp/test_ec_simple_runtime_contract.log
compile_and_run tests/integration/test_ecdsa_simple.pas tmp/test_ecdsa_simple_runtime_contract "$PASS_TOKEN" /tmp/test_ecdsa_simple_runtime_contract.log
compile_and_run tests/integration/test_asn1_module.pas tmp/test_asn1_module_runtime_contract "$PASS_TOKEN" /tmp/test_asn1_module_runtime_contract.log
compile_and_run tests/integration/test_ec_comprehensive.pas tmp/test_ec_comprehensive_runtime_contract "$PASS_TOKEN" /tmp/test_ec_comprehensive_runtime_contract.log
compile_and_run tests/integration/test_error_recovery.pas tmp/test_error_recovery_runtime_contract "$PASS_TOKEN" /tmp/test_error_recovery_runtime_contract.log
compile_and_run tests/integration/test_rsa_comprehensive.pas tmp/test_rsa_comprehensive_runtime_contract "$PASS_TOKEN" /tmp/test_rsa_comprehensive_runtime_contract.log
compile_and_run tests/integration/test_rsa_integration.pas tmp/test_rsa_integration_runtime_contract "$PASS_TOKEN" /tmp/test_rsa_integration_runtime_contract.log

echo '[PASS] integration simple runtime programs stay green'
