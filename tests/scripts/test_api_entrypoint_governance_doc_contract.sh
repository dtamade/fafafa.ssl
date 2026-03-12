#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOC="$ROOT_DIR/docs/reference/API_ENTRYPOINT_GOVERNANCE.md"

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

echo "[TEST] api entrypoint governance doc contract"

[[ -f "$DOC" ]] || fail "API entrypoint governance doc should exist"

assert_contains '## 推荐主入口'
assert_contains '## 兼容/底层入口'
assert_contains '## Deprecated / Bridge Surface'
assert_contains 'TSSLContextBuilder'
assert_contains 'TSSLFactory + TSSLConfig'
assert_contains 'TSSLConnector / TSSLStream'
assert_contains 'ISSLContext.ServerName'
assert_contains 'WolfSSL'

echo "[PASS] api entrypoint governance doc contract passed"
