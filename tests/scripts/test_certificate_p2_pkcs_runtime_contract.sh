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
  local out="$2"
  local token="$3"
  local log="$4"
  local compile_log="${log}.compile"
  mkdir -p "$(dirname "$out")" "$(dirname "$log")"

  if ! fpc -Fu./src "$src" -o"$out" >"$compile_log" 2>&1; then
    echo "[INFO] compile output for $src:"
    sed -n '1,220p' "$compile_log" || true
    fail "$src should compile"
  fi

  if ! "./$out" >"$log" 2>&1; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,240p' "$log" || true
    fail "$src should run successfully"
  fi

  if ! rg -F --quiet -- "$token" "$log"; then
    echo "[INFO] runtime output for $src:"
    sed -n '1,240p' "$log" || true
    fail "$src should print success marker"
  fi
}

compile_and_run tests/certificate/test_p2_pkcs12.pas tmp/runtime_contracts/test_p2_pkcs12_contract 'All tests PASSED!' tmp/runtime_contracts/test_p2_pkcs12_contract.log
compile_and_run tests/certificate/test_p2_pkcs7.pas tmp/runtime_contracts/test_p2_pkcs7_contract 'Result: ALL TESTS PASSED [OK]' tmp/runtime_contracts/test_p2_pkcs7_contract.log
compile_and_run tests/certificate/test_pkcs12_workflow.pas tmp/runtime_contracts/test_pkcs12_workflow_contract 'Result: ALL TESTS PASSED [OK]' tmp/runtime_contracts/test_pkcs12_workflow_contract.log
compile_and_run tests/certificate/test_x509_enterprise.pas tmp/runtime_contracts/test_x509_enterprise_contract '[PASS] x509 enterprise completed' tmp/runtime_contracts/test_x509_enterprise_contract.log
compile_and_run tests/certificate/test_p2_pkcs12_comprehensive.pas tmp/runtime_contracts/test_p2_pkcs12_comprehensive_contract '[PASS] pkcs12 comprehensive completed' tmp/runtime_contracts/test_p2_pkcs12_comprehensive_contract.log
compile_and_run tests/certificate/test_p2_pkcs7_boundary.pas tmp/runtime_contracts/test_p2_pkcs7_boundary_contract '[PASS] pkcs7 boundary completed' tmp/runtime_contracts/test_p2_pkcs7_boundary_contract.log
compile_and_run tests/certificate/test_p2_pkcs7_comprehensive.pas tmp/runtime_contracts/test_p2_pkcs7_comprehensive_contract '[PASS] pkcs7 comprehensive completed' tmp/runtime_contracts/test_p2_pkcs7_comprehensive_contract.log

echo '[PASS] certificate P2 PKCS runtime programs stay green'
