#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/lazbuild_all.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] lazbuild_all dynamic lazbuild resolution contract"

if ! rg -F --quiet -- 'LAZBUILD="${LAZBUILD:-$(command -v lazbuild || true)}"' "$SCRIPT"; then
  fail "script should support env override + PATH-based lazbuild discovery"
fi

if ! rg -F --quiet -- 'for candidate in "${LAZBUILD_CANDIDATES[@]}"; do' "$SCRIPT"; then
  fail "script should probe lazbuild candidates when PATH lookup misses"
fi

if ! rg -F --quiet -- '"$LAZBUILD" "$lpi"' "$SCRIPT"; then
  fail "script should execute lazbuild via resolved variable"
fi

if rg -F --quiet -- '/home/dtamade/freePascal/lazarus/lazbuild' "$SCRIPT"; then
  fail "script should not use hard-coded lazbuild path"
fi

echo "[PASS] lazbuild_all dynamic lazbuild resolution contract passed"
