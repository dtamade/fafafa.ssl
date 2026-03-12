#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_all_module_tests.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] run_all_module_tests unit output isolation contract"

if ! rg -F --quiet 'FPC_UNIT_OUTPUT_DIR=' "$SCRIPT"; then
  fail "script should define FPC_UNIT_OUTPUT_DIR default strategy"
fi

if ! rg -F --quiet -- '-FU"$FPC_UNIT_OUTPUT_DIR"' "$SCRIPT"; then
  fail "fpc compile command should include isolated -FU output dir"
fi

echo "[PASS] run_all_module_tests unit output isolation contract"
