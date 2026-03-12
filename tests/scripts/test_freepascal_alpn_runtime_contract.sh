#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TEST_FILE="tests/integration/test_freepascal_alpn_runtime.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal ALPN runtime contract"

rg -F --quiet -- "FAFAFA_ALPN_RUNTIME_HOSTS" "$TEST_FILE" || \
  fail "ALPN runtime integration should accept multi-host env override"

rg -F --quiet -- "FAFAFA_ALPN_RUNTIME_HOST" "$TEST_FILE" || \
  fail "ALPN runtime integration should accept single-host env override"

rg -F --quiet -- "FAFAFA_ALPN_RUNTIME_PROTOCOLS" "$TEST_FILE" || \
  fail "ALPN runtime integration should accept offered ALPN protocol override"

rg -F --quiet -- "WithALPN(" "$TEST_FILE" || \
  fail "ALPN runtime integration should configure ALPN on the client context"

rg -F --quiet -- "GetSelectedALPNProtocol" "$TEST_FILE" || \
  fail "ALPN runtime integration should assert negotiated ALPN"

rg -F --quiet -- "GetConnectionInfo" "$TEST_FILE" || \
  fail "ALPN runtime integration should verify connection info ALPN projection"

rg -F --quiet -- "Skip: no ALPN runtime hosts configured" "$TEST_FILE" || \
  fail "ALPN runtime integration should skip cleanly when hosts are not configured"

echo "[PASS] pure Pascal ALPN runtime contract stays enforced"
