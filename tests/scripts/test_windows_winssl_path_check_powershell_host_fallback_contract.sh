#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_windows_winssl_path_check_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] windows winssl path-check powershell host fallback contract"

if ! rg -F --quiet -- 'POWERSHELL_EXE="pwsh"' "$SCRIPT"; then
  fail "script should define default powershell host as pwsh"
fi

if ! rg -F --quiet -- 'if ! command -v "$POWERSHELL_EXE" >/dev/null 2>&1; then' "$SCRIPT"; then
  fail "script should probe default powershell host availability"
fi

if ! rg -F --quiet -- 'if command -v powershell >/dev/null 2>&1; then' "$SCRIPT"; then
  fail "script should fallback to powershell when pwsh is unavailable"
fi

if ! rg -F --quiet -- '$POWERSHELL_EXE -NoProfile' "$SCRIPT"; then
  fail "script should execute commands via resolved powershell host variable"
fi

echo "[PASS] windows winssl path-check powershell host fallback contract passed"
