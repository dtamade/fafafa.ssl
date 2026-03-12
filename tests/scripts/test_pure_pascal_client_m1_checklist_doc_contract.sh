#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOC="$ROOT_DIR/docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md"

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

echo "[TEST] pure pascal client m1 checklist doc contract"

[[ -f "$DOC" ]] || fail "pure Pascal client M1 checklist doc should exist"

assert_contains '## M1 Target'
assert_contains '## 已满足'
assert_contains '## 部分满足'
assert_contains '## 缺失'
assert_contains 'TLS 1.2 / 1.3'
assert_contains '证书链校验'
assert_contains 'hostname verification'
assert_contains '系统根证书'
assert_contains '自定义 CA / CA bundle'
assert_contains 'SNI'
assert_contains 'ALPN'
assert_contains '超时、取消、明确错误语义'
assert_contains '流式读写与关闭语义'
assert_contains '日志 / 握手失败原因 / 对端证书信息'

echo "[PASS] pure pascal client m1 checklist doc contract passed"
