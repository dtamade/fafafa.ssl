#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/validate_all_modules.ps1"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] validate_all_modules unit output isolation contract"

if ! rg -F --quiet -- '[string]$UnitOutputDir = ""' "$SCRIPT"; then
  fail "script should expose -UnitOutputDir parameter"
fi

if ! rg -F --quiet -- '$resolvedUnitOutputDir' "$SCRIPT"; then
  fail "script should resolve effective unit output directory"
fi

if ! rg -F --quiet -- '-FU$resolvedUnitOutputDir' "$SCRIPT"; then
  fail "fpc invocation should include -FU isolated unit output directory"
fi

echo "[PASS] validate_all_modules unit output isolation contract passed"
