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
  local out_path="$2"
  local token="$3"
  local log_path="$4"
  local compile_log="$log_path.compile"

  mkdir -p "$(dirname "$out_path")" "$(dirname "$log_path")"

  if ! fpc -Fu./src "$src" -o"$out_path" >"$compile_log" 2>&1; then
    echo "[INFO] compile output for $src:"
    sed -n '1,220p' "$compile_log" || true
    fail "$src should compile"
  fi

  if ! "./$out_path" >"$log_path" 2>&1; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,240p' "$log_path" || true
    fail "$src should run successfully under redirected stdin"
  fi

  if ! rg -F --quiet -- "$token" "$log_path"; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,240p' "$log_path" || true
    fail "$src should print success marker"
  fi
}

compile_and_run tests/certificate/test_cert_utils_simple.pas tmp/runtime_contracts/test_cert_utils_simple_contract 'Test completed successfully!' tmp/runtime_contracts/test_cert_utils_simple_contract.log
compile_and_run tests/certificate/test_cert_utils.pas tmp/runtime_contracts/test_cert_utils_contract 'Result: All tests passed!' tmp/runtime_contracts/test_cert_utils_contract.log
compile_and_run tests/certificate/test_certificate_chain_methods.pas tmp/runtime_contracts/test_certificate_chain_methods_contract 'ALL CERTIFICATE CHAIN TESTS PASSED!' tmp/runtime_contracts/test_certificate_chain_methods_contract.log
compile_and_run tests/certificate/test_cert_verification_failures.pas tmp/runtime_contracts/test_cert_verification_failures_contract 'Result: ALL TESTS PASSED!' tmp/runtime_contracts/test_cert_verification_failures_contract.log
compile_and_run tests/certificate/test_cert_store.pas tmp/runtime_contracts/test_cert_store_contract 'All tests PASSED!' tmp/runtime_contracts/test_cert_store_contract.log
compile_and_run tests/certificate/test_certificate_unit.pas tmp/runtime_contracts/test_certificate_unit_contract '✅ ALL TESTS PASSED!' tmp/runtime_contracts/test_certificate_unit_contract.log
compile_and_run tests/certificate/test_certificate_real.pas tmp/runtime_contracts/test_certificate_real_contract '✅ ALL TESTS PASSED!' tmp/runtime_contracts/test_certificate_real_contract.log
compile_and_run tests/certificate/test_cert_utils_enterprise.pas tmp/runtime_contracts/test_cert_utils_enterprise_contract '[PASS] cert utils enterprise completed' tmp/runtime_contracts/test_cert_utils_enterprise_contract.log
compile_and_run tests/certificate/test_cert_utils_try.pas tmp/runtime_contracts/test_cert_utils_try_contract '[PASS] cert utils try completed' tmp/runtime_contracts/test_cert_utils_try_contract.log
compile_and_run tests/certificate/test_p2_pkcs12_create_parse.pas tmp/runtime_contracts/test_p2_pkcs12_create_parse_contract '[PASS] p2 pkcs12 create/parse completed' tmp/runtime_contracts/test_p2_pkcs12_create_parse_contract.log
compile_and_run tests/certificate/test_p2_pkcs7_encrypt_decrypt.pas tmp/runtime_contracts/test_p2_pkcs7_encrypt_decrypt_contract '[PASS] p2 pkcs7 encrypt/decrypt completed' tmp/runtime_contracts/test_p2_pkcs7_encrypt_decrypt_contract.log
compile_and_run tests/certificate/test_p2_pkcs7_sign_verify.pas tmp/runtime_contracts/test_p2_pkcs7_sign_verify_contract '[PASS] p2 pkcs7 sign/verify completed' tmp/runtime_contracts/test_p2_pkcs7_sign_verify_contract.log
compile_and_run tests/certificate/test_pkcs7_data_debug.pas tmp/runtime_contracts/test_pkcs7_data_debug_contract '[PASS] pkcs7 data debug completed' tmp/runtime_contracts/test_pkcs7_data_debug_contract.log

echo '[PASS] certificate non-P2 runtime programs stay green'
