#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOC="$ROOT_DIR/docs/reference/ARCHITECTURE.md"

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

echo "[TEST] architecture api canon doc contract"

assert_contains '## API Canon'
assert_contains '### Core API'
assert_contains '### Advanced API'
assert_contains '### Backend-Specific API'
assert_contains '`TSSLContextBuilder` 是唯一推荐主入口'
assert_contains '`TSSLFactory + TSSLConfig` 仅保留为兼容/底层入口'
assert_contains '`TSSLConnector` / `TSSLStream`'
assert_contains '`UsePKCS11(...)` 只替代私钥来源'
assert_contains '纯 Pascal 后端'
assert_contains 'HTTPS/TLS 客户端生产可用'
assert_contains 'Linux + Windows'

echo "[PASS] architecture api canon doc contract passed"
