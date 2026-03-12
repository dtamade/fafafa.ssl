#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/validate_all_modules.ps1"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] validate_all_modules dynamic project root contract"

if ! rg -F --quiet -- '[string]$ProjectRoot = ""' "$SCRIPT"; then
  fail "script should expose -ProjectRoot parameter"
fi

if ! rg -F --quiet -- '$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path' "$SCRIPT"; then
  fail "script should derive scriptDir from invocation path"
fi

if rg -F --quiet -- 'D:\projects\Pascal\lazarus\My\libs\fafafa.ssl' "$SCRIPT"; then
  fail "script should not use hard-coded project root path"
fi

echo "[PASS] validate_all_modules dynamic project root contract passed"
