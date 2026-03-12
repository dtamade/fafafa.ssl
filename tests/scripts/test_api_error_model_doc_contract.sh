#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOC="$ROOT_DIR/docs/reference/API_ERROR_MODEL.md"

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

echo "[TEST] api error model doc contract"

[[ -f "$DOC" ]] || fail "API error model doc should exist"

assert_contains '## Core API'
assert_contains '## Advanced API'
assert_contains 'TSSLOperationResult'
assert_contains 'TSSLDataResult'
assert_contains 'ESSLException'
assert_contains 'warning'
assert_contains 'unsupported'
assert_contains 'configuration'

echo "[PASS] api error model doc contract passed"
