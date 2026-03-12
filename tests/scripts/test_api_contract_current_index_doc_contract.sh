#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOC="$ROOT_DIR/docs/reference/API_CONTRACT_CURRENT_INDEX.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local pattern="$1"
  if ! rg -F --quiet -- "$pattern" "$DOC"; then
    echo "[INFO] missing pattern '$pattern' in $DOC"
    sed -n '1,260p' "$DOC" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] api contract current index doc contract"

[[ -f "$DOC" ]] || fail "API contract current index doc should exist"

assert_contains '## Core API Contracts'
assert_contains '## Advanced API Contracts'
assert_contains '## Backend-Specific Contracts'
assert_contains 'backend resolution'
assert_contains 'library-scope vs request/context-scope'
assert_contains 'ServerName'
assert_contains 'file / PEM / PKCS11 precedence'
assert_contains '纯 Pascal'
assert_contains '路线图入口'

echo "[PASS] api contract current index doc contract passed"
