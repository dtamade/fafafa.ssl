#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SOURCE_FILE="tests/openssl/test_ocsp_connection_verification_regression.pas"
BINARY_REL="tmp/test_ocsp_connection_verification_regression"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] focused compile zero-noise contract (ocsp regression)"

COMPILE_LOG="$(mktemp)"
RUN_LOG="$(mktemp)"
trap 'rm -f "$COMPILE_LOG" "$RUN_LOG"' EXIT

if ! (
  cd "$ROOT_DIR"
  fpc -Fu./src "$SOURCE_FILE" -o"$BINARY_REL" >"$COMPILE_LOG" 2>&1
); then
  echo "[INFO] compile output:"
  sed -n '1,260p' "$COMPILE_LOG" || true
  fail "focused compile command failed for ocsp regression entrypoint"
fi

if rg -n --no-heading --color never "Warning:|Note:|warning\(s\) issued|note\(s\) issued" "$COMPILE_LOG" >/dev/null; then
  echo "[INFO] focused compile noise detected:"
  rg -n --no-heading --color never "Warning:|Note:|warning\(s\) issued|note\(s\) issued" "$COMPILE_LOG" || true
  fail "focused compile should be 0 warning and 0 note for ocsp regression entrypoint"
fi

if ! (
  cd "$ROOT_DIR"
  "./$BINARY_REL" >"$RUN_LOG" 2>&1
); then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "focused runtime command failed for ocsp regression entrypoint"
fi

if ! rg -F --quiet -- "OCSP Connection Verification Regression Test" "$RUN_LOG"; then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "runtime output missing ocsp regression header marker"
fi

PASSED_LINE="$(rg -n --no-heading --color never '^Passed:[[:space:]]*[0-9]+$' "$RUN_LOG" | tail -n 1 || true)"
FAILED_LINE="$(rg -n --no-heading --color never '^Failed:[[:space:]]*[0-9]+$' "$RUN_LOG" | tail -n 1 || true)"

if [[ -z "$PASSED_LINE" ]] || [[ -z "$FAILED_LINE" ]]; then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "runtime output missing Passed/Failed summary lines"
fi

PASSED_COUNT="$(printf '%s\n' "$PASSED_LINE" | rg -o '[0-9]+' | tail -n 1 || true)"
FAILED_COUNT="$(printf '%s\n' "$FAILED_LINE" | rg -o '[0-9]+' | tail -n 1 || true)"

if [[ ! "$PASSED_COUNT" =~ ^[0-9]+$ ]] || [[ ! "$FAILED_COUNT" =~ ^[0-9]+$ ]]; then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "summary counters should be numeric"
fi

if [[ "$FAILED_COUNT" -ne 0 ]]; then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "runtime summary reported failed test cases"
fi

if [[ "$PASSED_COUNT" -lt 1 ]]; then
  echo "[INFO] runtime output:"
  sed -n '1,260p' "$RUN_LOG" || true
  fail "runtime summary should report at least one passed case"
fi

echo "[INFO] ocsp summary: passed=$PASSED_COUNT failed=$FAILED_COUNT"
echo "[PASS] focused ocsp regression compile remains zero-noise and runtime summary is valid"
