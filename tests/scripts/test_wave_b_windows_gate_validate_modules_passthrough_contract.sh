#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_windows_gate.ps1"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave b windows gate validate modules passthrough contract"

if ! rg -F --quiet -- "./scripts/validate_all_modules.ps1 -ProjectRoot '\$ProjectRoot' -UnitOutputDir '\$OutputDir/wave_b_windows_validate_units_" "$SCRIPT"; then
  fail "modules step should passthrough -ProjectRoot and -UnitOutputDir"
fi

echo "[PASS] wave b windows gate validate modules passthrough contract passed"
