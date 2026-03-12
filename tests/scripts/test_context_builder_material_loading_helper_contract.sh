#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FILE="$ROOT_DIR/src/fafafa.ssl.context.builder.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local pattern="$1"
  if ! rg -F --quiet -- "$pattern" "$FILE"; then
    echo "[INFO] missing pattern '$pattern' in $FILE"
    sed -n '180,340p' "$FILE" || true
    sed -n '1330,1510p' "$FILE" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] context builder material loading helper contract"

assert_contains 'procedure ApplyResolvedContextConfiguration(AContext: ISSLContext;'
assert_contains 'ARequireServerIdentity: Boolean);'
assert_contains 'ApplyResolvedContextConfiguration(Result, SelectedBackend, False);'
assert_contains 'ApplyResolvedContextConfiguration(Result, SelectedBackend, True);'

echo "[PASS] context builder material loading helper contract passed"
