#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

BIN="tmp/test_module_headers_quick_contract"
LOG="/tmp/test_module_headers_quick_contract.log"

if ! fpc -Fu./src tests/test_module_headers_quick.pas -o"$BIN" >/tmp/test_module_headers_quick_contract.compile.log 2>&1; then
  echo '[INFO] compile output:'
  sed -n '1,260p' /tmp/test_module_headers_quick_contract.compile.log || true
  fail 'test_module_headers_quick should compile'
fi

if ! "./$BIN" >"$LOG" 2>&1; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_module_headers_quick should run successfully'
fi

if ! rg -F --quiet -- '✓✓✓ 所有模块头文件验证通过! ✓✓✓' "$LOG"; then
  echo '[INFO] runtime output:'
  sed -n '1,260p' "$LOG" || true
  fail 'test_module_headers_quick should report success marker'
fi

echo '[PASS] test_module_headers_quick stays green at runtime'
