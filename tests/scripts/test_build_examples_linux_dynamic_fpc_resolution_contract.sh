#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/build_examples_linux.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] build_examples_linux dynamic fpc resolution contract"

if ! rg -F --quiet -- 'FPC="${FPC:-$(command -v fpc || true)}"' "$SCRIPT"; then
  fail "script should resolve FPC from env override + PATH lookup"
fi

if ! rg -F --quiet -- 'for candidate in "${FPC_CANDIDATES[@]}"; do' "$SCRIPT"; then
  fail "script should probe fallback FPC candidates"
fi

if ! rg -F --quiet -- 'FPC_UNITS="${FPC_UNITS:-}"' "$SCRIPT"; then
  fail "script should expose optional FPC_UNITS override"
fi

if ! rg -F --quiet -- 'for candidate in "${FPC_UNITS_CANDIDATES[@]}"; do' "$SCRIPT"; then
  fail "script should probe FPC unit directory candidates when override is empty"
fi

if ! rg -F --quiet -- 'if "$FPC" "${UNIT_PATHS[@]}" -FE"$OUT_DIR" "$src"' "$SCRIPT"; then
  fail "script should execute compile command via resolved FPC and array-based unit paths"
fi

if ! rg -F --quiet -- 'BUILD_LOG="$ROOT_DIR/tmp/build_examples_linux_${RUN_ID}.log"' "$SCRIPT"; then
  fail "script should write build logs under project tmp with run-scoped name"
fi

if rg -F --quiet -- '/tmp/example_build.log' "$SCRIPT"; then
  fail "script should not use shared fixed /tmp example build log path"
fi

echo "[PASS] build_examples_linux dynamic fpc resolution contract passed"
