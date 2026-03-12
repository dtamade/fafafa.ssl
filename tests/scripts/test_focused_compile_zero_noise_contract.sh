#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SOURCE_FILE="tests/openssl/test_openssl_chain_issuer_selection.pas"
BINARY_REL="tmp/test_openssl_chain_issuer_selection"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] focused compile zero-noise contract"

COMPILE_LOG="$(mktemp)"
RUN_LOG="$(mktemp)"
trap 'rm -f "$COMPILE_LOG" "$RUN_LOG"' EXIT

if ! (
  cd "$ROOT_DIR"
  fpc -Fu./src "$SOURCE_FILE" -o"$BINARY_REL" >"$COMPILE_LOG" 2>&1
); then
  echo "[INFO] compile output:"
  sed -n '1,260p' "$COMPILE_LOG" || true
  fail "focused compile command failed"
fi

if rg -n --no-heading --color never "Warning:|Note:|warning\(s\) issued|note\(s\) issued" "$COMPILE_LOG" >/dev/null; then
  echo "[INFO] focused compile noise detected:"
  rg -n --no-heading --color never "Warning:|Note:|warning\(s\) issued|note\(s\) issued" "$COMPILE_LOG" || true
  fail "focused compile should be 0 warning and 0 note"
fi

if ! (
  cd "$ROOT_DIR"
  "./$BINARY_REL" >"$RUN_LOG" 2>&1
); then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "focused runtime command failed"
fi

if ! rg -F --quiet -- "All issuer selection tests passed." "$RUN_LOG"; then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "focused runtime output missing PASS marker"
fi

echo "[PASS] focused compile remains zero-noise and runtime marker is present"
