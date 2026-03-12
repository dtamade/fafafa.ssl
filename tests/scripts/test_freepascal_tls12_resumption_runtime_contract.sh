#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TEST_FILE="tests/integration/test_freepascal_tls12_resumption_runtime.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal TLS1.2 resumption runtime contract"

rg -F --quiet -- "FAFAFA_TLS12_RESUMPTION_HOSTS" "$TEST_FILE" || \
  fail "TLS1.2 resumption runtime integration should accept multi-host env override"

rg -F --quiet -- "FAFAFA_TLS12_RESUMPTION_HOST" "$TEST_FILE" || \
  fail "TLS1.2 resumption runtime integration should accept single-host env override"

rg -F --quiet -- "FAFAFA_TLS12_RESUMPTION_REQUIRE_REUSE" "$TEST_FILE" || \
  fail "TLS1.2 resumption runtime integration should support strict reuse requirement override"

rg -F --quiet -- "IsSessionReused" "$TEST_FILE" || \
  fail "TLS1.2 resumption runtime integration should assert reused handshake on second connect"

rg -F --quiet -- "Skip: no TLS1.2 resumption hosts configured" "$TEST_FILE" || \
  fail "TLS1.2 resumption runtime integration should skip cleanly when hosts are not configured"

echo "[PASS] pure Pascal TLS1.2 resumption runtime contract stays enforced"
