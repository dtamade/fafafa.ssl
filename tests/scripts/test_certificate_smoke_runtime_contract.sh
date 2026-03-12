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
    sed -n '1,220p' "$log_path" || true
    fail "$src should run successfully"
  fi

  if ! rg -F --quiet -- "$token" "$log_path"; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,220p' "$log_path" || true
    fail "$src should print success marker"
  fi
}

compile_and_run tests/certificate/test_pem_simple.pas tmp/runtime_contracts/test_pem_simple_contract '[PASS] PEM module compiled successfully' tmp/runtime_contracts/test_pem_simple.log
compile_and_run tests/certificate/test_p2_pkcs12_simple.pas tmp/runtime_contracts/test_p2_pkcs12_simple_contract 'All tests PASSED!' tmp/runtime_contracts/test_p2_pkcs12_simple.log
compile_and_run tests/certificate/test_p2_pkcs7_data.pas tmp/runtime_contracts/test_p2_pkcs7_data_contract '[PASS] p2 pkcs7 data completed' tmp/runtime_contracts/test_p2_pkcs7_data.log
compile_and_run tests/certificate/test_ocsp_validation.pas tmp/runtime_contracts/test_ocsp_validation_contract '[PASS] ocsp validation completed' tmp/runtime_contracts/test_ocsp_validation.log
compile_and_run tests/certificate/test_pkcs7_sign_verify_workflow.pas tmp/runtime_contracts/test_pkcs7_sign_verify_workflow_contract 'Result: ALL TESTS PASSED [OK]' tmp/runtime_contracts/test_pkcs7_sign_verify_workflow.log
compile_and_run tests/certificate/test_pkcs7_workflow.pas tmp/runtime_contracts/test_pkcs7_workflow_contract 'PKCS#7 module is production-ready' tmp/runtime_contracts/test_pkcs7_workflow.log
compile_and_run tests/certificate/test_cert_verify.pas tmp/runtime_contracts/test_cert_verify_contract 'All tests completed successfully!' tmp/runtime_contracts/test_cert_verify.log
compile_and_run tests/certificate/test_certstore_unit.pas tmp/runtime_contracts/test_certstore_unit_contract 'Success rate: 100%' tmp/runtime_contracts/test_certstore_unit.log
compile_and_run tests/certificate/test_tsa_api.pas tmp/runtime_contracts/test_tsa_api_contract 'ALL TSA API TESTS PASSED!' tmp/runtime_contracts/test_tsa_api.log

echo '[PASS] certificate smoke programs stay green at runtime'
