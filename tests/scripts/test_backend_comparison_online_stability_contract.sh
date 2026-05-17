#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FILE="$ROOT_DIR/tests/integration/test_backend_comparison.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] backend comparison online stability contract"

[[ -f "$FILE" ]] || fail "missing file: tests/integration/test_backend_comparison.pas"

if ! rg -F --quiet -- 'function IsExpectedNegativePathFailure' "$FILE"; then
  fail "test_backend_comparison.pas should classify negative-path SSL exceptions explicitly"
fi

if ! rg -F --quiet -- 'function GetHTTPStatusClass' "$FILE"; then
  fail "test_backend_comparison.pas should compare live HTTP responses by normalized status class, not byte-for-byte equality"
fi

if rg -F --quiet -- "Test('数据完整性一致 (MD5)'" "$FILE"; then
  fail "test_backend_comparison.pas should not require exact MD5 equality for live internet responses"
fi

if rg -F --quiet -- "Test('数据长度相同'" "$FILE"; then
  fail "test_backend_comparison.pas should not require exact byte-length equality for live internet responses"
fi

if rg -F --quiet -- "Test('中等数据完整性一致 (MD5)'" "$FILE"; then
  fail "test_backend_comparison.pas should not require exact medium-response MD5 equality for live internet responses"
fi

expected_fail_count="$(rg -F --count -- 'IsExpectedNegativePathFailure(E)' "$FILE")"
if [[ "$expected_fail_count" -lt 4 ]]; then
  fail "negative-path helper should be used for both WinSSL/OpenSSL HTTP-port and SSL3 expected-failure assertions"
fi

echo "[PASS] backend comparison online stability contract passed"
