#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_b101_validation_playbook.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave_c b101 validation playbook fpc host passthrough contract"

if ! rg -F --quiet -- "FPC_EXE=\"\${FAFAFA_FPC_EXE:-fpc}\"" "$SCRIPT"; then
  fail "script should define FPC_EXE override variable"
fi

if ! rg -F --quiet -- "python3 scripts/compile_all_modules.py --unit-output-dir '\$COMPILE_UNIT_OUTPUT_DIR' --fpc-exe '\$FPC_EXE'" "$SCRIPT"; then
  fail "full-gate compile step should passthrough --fpc-exe override"
fi

if ! rg -F --quiet -- "FAFAFA_FPC_EXE='\$FPC_EXE' FAFAFA_FPC_UNIT_OUTPUT_DIR='\$MODULE_UNIT_OUTPUT_DIR' FAFAFA_TEST_BIN_DIR='\$MODULE_BIN_OUTPUT_DIR'" "$SCRIPT"; then
  fail "full-gate module step should passthrough FAFAFA_FPC_EXE"
fi

if ! rg -F --quiet -- "mkdir -p tests/benchmarks/bin && '\$FPC_EXE' -Mobjfpc -Sh -O2" "$SCRIPT"; then
  fail "benchmark compile step should use FAFAFA_FPC_EXE override"
fi

echo "[PASS] wave_c b101 validation playbook fpc host passthrough contract passed"
