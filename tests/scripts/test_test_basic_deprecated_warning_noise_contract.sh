#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

LOG_FILE="tmp/test_basic_deprecated_warning_contract.log"
mkdir -p tmp

fpc -Fu./src -Fi./src tests/examples/test_basic.pas -otmp/test_examples_test_basic_smoke >"$LOG_FILE" 2>&1 || true

if rg -n "test_basic\\.pas.*deprecated" "$LOG_FILE"; then
  echo "[FAIL] test_basic should not emit file-local deprecated warnings"
  exit 1
fi

echo "[PASS] test_basic deprecated warning noise is localized"
