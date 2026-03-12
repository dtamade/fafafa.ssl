#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOC="$ROOT_DIR/docs/reference/API_CAPABILITY_STRATEGY.md"

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

echo "[TEST] api capability strategy doc contract"

[[ -f "$DOC" ]] || fail "API capability strategy doc should exist"

assert_contains '## Core API'
assert_contains '## Advanced API'
assert_contains 'TSSLBackendCapabilities'
assert_contains 'RequiresExternalLibrary'
assert_contains 'SupportsSystemCertStore'
assert_contains 'SupportsPKCS11'
assert_contains 'unsupported'
assert_contains 'fallback'

echo "[PASS] api capability strategy doc contract passed"
