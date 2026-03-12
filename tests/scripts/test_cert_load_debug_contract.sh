#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

OUT="tmp/runtime_contracts/test_cert_load_debug_contract"
LOG="tmp/runtime_contracts/test_cert_load_debug_contract.compile.log"
mkdir -p "$(dirname "$OUT")"

if ! fpc -Fu./src tests/certificate/test_cert_load_debug.pas -o"$OUT" >"$LOG" 2>&1; then
  echo "[INFO] compile output for tests/certificate/test_cert_load_debug.pas:"
  sed -n '1,220p' "$LOG" || true
  fail 'test_cert_load_debug should compile as environment probe'
fi

echo '[PASS] test_cert_load_debug remains compile-only due environment-specific certificate paths'
