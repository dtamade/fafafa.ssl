#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

# Integration workflow should compile and run when framework path is provided.
INTEGRATION_BIN='tmp/test_integration_cross_module_workflow_contract'
INTEGRATION_LOG='/tmp/test_integration_cross_module_workflow_contract.log'
if ! fpc -Fu./src -Fu./tests/framework tests/integration/test_integration_cross_module_workflow.pas -o"$INTEGRATION_BIN" >/tmp/test_integration_cross_module_workflow_contract.compile.log 2>&1; then
  echo '[INFO] integration compile output:'
  sed -n '1,260p' /tmp/test_integration_cross_module_workflow_contract.compile.log || true
  fail 'test_integration_cross_module_workflow should compile with framework path on Linux'
fi
if ! "./$INTEGRATION_BIN" >"$INTEGRATION_LOG" 2>&1; then
  echo '[INFO] integration runtime output:'
  sed -n '1,260p' "$INTEGRATION_LOG" || true
  fail 'test_integration_cross_module_workflow should run successfully'
fi
if ! rg -F --quiet -- '[PASS] integration cross-module workflow completed' "$INTEGRATION_LOG"; then
  echo '[INFO] integration runtime output:'
  sed -n '1,260p' "$INTEGRATION_LOG" || true
  fail 'test_integration_cross_module_workflow should print completion marker'
fi

# PKCS11 SoftHSM test should compile and either pass or exit with explicit SKIP semantics.
PKCS11_BIN='tmp/test_pkcs11_softhsm_contract'
PKCS11_LOG='/tmp/test_pkcs11_softhsm_contract.log'
if ! fpc -Fu./src tests/pkcs11/test_pkcs11_softhsm.pas -o"$PKCS11_BIN" >/tmp/test_pkcs11_softhsm_contract.compile.log 2>&1; then
  echo '[INFO] pkcs11 compile output:'
  sed -n '1,260p' /tmp/test_pkcs11_softhsm_contract.compile.log || true
  fail 'test_pkcs11_softhsm should compile'
fi
set +e
"./$PKCS11_BIN" >"$PKCS11_LOG" 2>&1
STATUS=$?
set -e
if rg -F --quiet -- '[FAIL]' "$PKCS11_LOG" && ! rg -F --quiet -- '[SKIP]' "$PKCS11_LOG"; then
  echo '[INFO] pkcs11 runtime output:'
  sed -n '1,260p' "$PKCS11_LOG" || true
  fail 'pkcs11 runtime should not report raw FAIL when environment dependency is missing'
fi
if [[ "$STATUS" -ne 0 ]] && ! rg -F --quiet -- '[SKIP]' "$PKCS11_LOG"; then
  echo '[INFO] pkcs11 runtime output:'
  sed -n '1,260p' "$PKCS11_LOG" || true
  fail 'non-zero PKCS11 exit is only allowed with explicit [SKIP] semantics'
fi

echo '[PASS] integration + pkcs11 runtime semantics are explicit'
