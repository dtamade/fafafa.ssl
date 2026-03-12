#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TEST_FILE="tests/integration/test_freepascal_alpn_runtime.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal ALPN empty-negotiation contract"

rg -F --quiet -- "FAFAFA_ALPN_RUNTIME_ALLOW_EMPTY" "$TEST_FILE" || \
  fail "ALPN runtime integration should expose an allow-empty override"

rg -F --quiet -- "ALPN runtime handshake should negotiate a non-empty ALPN protocol" "$TEST_FILE" || \
  fail "ALPN runtime integration should still keep the strict non-empty assertion path when allow-empty is disabled"

echo "[PASS] pure Pascal ALPN empty-negotiation contract stays enforced"
