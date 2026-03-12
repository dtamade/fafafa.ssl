#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_windows_gate.ps1"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave b windows gate powershell host fallback contract"

if ! rg -F --quiet -- 'Get-Command $PowerShellExe -ErrorAction SilentlyContinue' "$SCRIPT"; then
  fail "script should probe pwsh host availability"
fi

if ! rg -F --quiet -- 'Get-Command "powershell" -ErrorAction SilentlyContinue' "$SCRIPT"; then
  fail "script should fallback to powershell host"
fi

if ! rg -F --quiet -- '& $PowerShellExe -ExecutionPolicy Bypass -Command $Command' "$SCRIPT"; then
  fail "step executor should use resolved powershell host variable"
fi

echo "[PASS] wave b windows gate powershell host fallback contract passed"
