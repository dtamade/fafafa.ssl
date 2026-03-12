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

TOKEN='All tests PASSED!'
compile_and_run tests/certificate/test_p2_cms.pas tmp/runtime_contracts/test_p2_cms_contract "$TOKEN" tmp/runtime_contracts/test_p2_cms_contract.log
compile_and_run tests/certificate/test_p2_ct.pas tmp/runtime_contracts/test_p2_ct_contract "$TOKEN" tmp/runtime_contracts/test_p2_ct_contract.log
compile_and_run tests/certificate/test_p2_ocsp.pas tmp/runtime_contracts/test_p2_ocsp_contract "$TOKEN" tmp/runtime_contracts/test_p2_ocsp_contract.log
compile_and_run tests/certificate/test_p2_ts.pas tmp/runtime_contracts/test_p2_ts_contract "$TOKEN" tmp/runtime_contracts/test_p2_ts_contract.log
compile_and_run tests/certificate/test_p2_cms_boundary.pas tmp/runtime_contracts/test_p2_cms_boundary_contract '[PASS] cms boundary completed' tmp/runtime_contracts/test_p2_cms_boundary_contract.log
compile_and_run tests/certificate/test_p2_cms_comprehensive.pas tmp/runtime_contracts/test_p2_cms_comprehensive_contract '[PASS] cms comprehensive completed' tmp/runtime_contracts/test_p2_cms_comprehensive_contract.log
compile_and_run tests/certificate/test_p2_ct_comprehensive.pas tmp/runtime_contracts/test_p2_ct_comprehensive_contract '[PASS] ct comprehensive completed' tmp/runtime_contracts/test_p2_ct_comprehensive_contract.log
compile_and_run tests/certificate/test_p2_ocsp_comprehensive.pas tmp/runtime_contracts/test_p2_ocsp_comprehensive_contract '[PASS] ocsp comprehensive completed' tmp/runtime_contracts/test_p2_ocsp_comprehensive_contract.log
compile_and_run tests/certificate/test_p2_ts_comprehensive.pas tmp/runtime_contracts/test_p2_ts_comprehensive_contract '[PASS] ts comprehensive completed' tmp/runtime_contracts/test_p2_ts_comprehensive_contract.log

echo '[PASS] certificate P2 core runtime programs stay green'
