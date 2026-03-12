#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/compile_all.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] compile_all dynamic fpc resolution contract"

if ! rg -F --quiet -- 'FPC="${FPC:-$(command -v fpc || true)}"' "$SCRIPT"; then
  fail "script should resolve FPC from env override + PATH lookup"
fi

if ! rg -F --quiet -- 'for candidate in "${FPC_CANDIDATES[@]}"; do' "$SCRIPT"; then
  fail "script should probe fallback FPC candidates"
fi

if ! rg -F --quiet -- 'FPC_UNITS="${FPC_UNITS:-$HOME/freePascal/fpc/units/x86_64-linux}"' "$SCRIPT"; then
  fail "script should expose configurable FPC_UNITS default"
fi

if ! rg -F --quiet -- '"$FPC" $FPC_FLAGS "$file"' "$SCRIPT"; then
  fail "script should invoke compiler through resolved FPC variable"
fi

if rg -F --quiet -- '/home/dtamade/freePascal/fpc' "$SCRIPT"; then
  fail "script should not keep hard-coded user-specific FPC path"
fi

echo "[PASS] compile_all dynamic fpc resolution contract passed"
