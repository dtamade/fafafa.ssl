#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/fix_unit_names.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] fix_unit_names dynamic project root contract"

if ! rg -F --quiet -- 'SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"' "$SCRIPT"; then
  fail "script should define SCRIPT_DIR from invocation path"
fi

if ! rg -F --quiet -- 'PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"' "$SCRIPT"; then
  fail "script should derive PROJECT_ROOT from script directory"
fi

if ! rg -F --quiet -- 'cd "$PROJECT_ROOT"' "$SCRIPT"; then
  fail "script should cd into dynamically resolved project root"
fi

if rg -F --quiet -- '/home/dtamade/projects/fafafa.ssl' "$SCRIPT"; then
  fail "script should not use hard-coded project root path"
fi

echo "[PASS] fix_unit_names dynamic project root contract passed"
