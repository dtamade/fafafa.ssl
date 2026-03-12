#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
GUIDE="$ROOT_DIR/docs/guides/PKCS11_USER_GUIDE.md"
README_MAIN="$ROOT_DIR/README.md"
README_DOCS="$ROOT_DIR/docs/README.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,220p' "$file" || true
    fail "expected pattern not found"
  fi
}

assert_not_contains() {
  local file="$1"
  local pattern="$2"
  if rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] unexpected pattern '$pattern' in $file"
    sed -n '1,220p' "$file" || true
    fail "unexpected pattern found"
  fi
}

echo "[TEST] pkcs11 builder docs current api contract"

assert_not_contains "$GUIDE" '.WithPKCS11Key('
assert_not_contains "$GUIDE" '.ForServer'
assert_not_contains "$GUIDE" '.Build;'
assert_contains "$GUIDE" '.UsePKCS11('
assert_contains "$GUIDE" '.BuildServer;'
assert_contains "$GUIDE" '只替代私钥来源'
assert_contains "$GUIDE" '仍需通过 `WithCertificate` 或 `WithCertificatePEM` 提供证书'

assert_contains "$README_MAIN" 'UsePKCS11'
assert_contains "$README_MAIN" '只替代私钥来源'
assert_contains "$README_DOCS" 'UsePKCS11'
assert_contains "$README_DOCS" '只替代私钥来源'

echo "[PASS] pkcs11 builder docs current api contract passed"
