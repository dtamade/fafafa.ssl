#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

BIN="tmp/test_all_modules_inventory_contract"
fpc -Fu./src tests/test_all_modules_comprehensive.pas -o"$BIN" >/tmp/module-inventory-compile.log 2>&1 || {
  echo '[INFO] compile output:'
  sed -n '1,220p' /tmp/module-inventory-compile.log || true
  fail 'inventory program failed to compile'
}

OUT="./$BIN"
RUN_OUT="$($OUT 2>&1)"

if [[ "$RUN_OUT" != *"Total Modules: 63"* ]]; then
  echo "$RUN_OUT"
  fail 'inventory output should report total module count'
fi

if [[ "$RUN_OUT" != *"types  [P1]"* ]] || [[ "$RUN_OUT" != *"rand  [P1]"* ]]; then
  echo "$RUN_OUT"
  fail 'inventory output should include populated module names and priorities'
fi

echo '[PASS] module inventory runtime output stays populated'
