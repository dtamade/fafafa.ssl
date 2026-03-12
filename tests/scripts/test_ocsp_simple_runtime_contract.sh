#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

OUT="tmp/runtime_contracts/test_ocsp_simple_contract"
LOG="tmp/runtime_contracts/test_ocsp_simple.log"
COMPILE_LOG="${LOG}.compile"
mkdir -p "$(dirname "$OUT")" "$(dirname "$LOG")"

if ! fpc -Fu./src tests/certificate/test_ocsp_simple.pas -o"$OUT" >"$COMPILE_LOG" 2>&1; then
  echo "[INFO] compile output for tests/certificate/test_ocsp_simple.pas:"
  sed -n '1,220p' "$COMPILE_LOG" || true
  fail 'test_ocsp_simple should compile'
fi

if ! "./$OUT" >"$LOG" 2>&1; then
  echo "[INFO] runtime output for tests/certificate/test_ocsp_simple.pas:"
  sed -n '1,220p' "$LOG" || true
  fail 'test_ocsp_simple should run successfully under redirected stdin'
fi

if ! rg -F --quiet -- '[PASS] ocsp simple completed' "$LOG"; then
  echo "[INFO] runtime output for tests/certificate/test_ocsp_simple.pas:"
  sed -n '1,220p' "$LOG" || true
  fail 'test_ocsp_simple should print stable completion marker'
fi

echo '[PASS] test_ocsp_simple stays green at runtime'
