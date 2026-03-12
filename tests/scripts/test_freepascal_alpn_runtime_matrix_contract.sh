#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TEST_FILE="tests/integration/test_freepascal_alpn_runtime.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal ALPN runtime matrix contract"

rg -F --quiet -- "FAFAFA_ALPN_RUNTIME_HOSTS" "$TEST_FILE" || \
  fail "ALPN runtime integration should accept a multi-host env override"

rg -F --quiet -- "FAFAFA_ALPN_RUNTIME_PROTOCOLS" "$TEST_FILE" || \
  fail "ALPN runtime integration should accept offered protocol overrides"

rg -F --quiet -- "FAFAFA_ALPN_RUNTIME_EXPECTED_PROTOCOL" "$TEST_FILE" || \
  fail "ALPN runtime integration should accept an expected negotiated protocol override"

rg -F --quiet -- "HostCandidatesToString" "$TEST_FILE" || \
  fail "ALPN runtime integration should print matrix host summaries"

rg -F --quiet -- "for I := 0 to LHosts.Count - 1 do" "$TEST_FILE" || \
  fail "ALPN runtime integration should iterate across multiple hosts"

rg -F --quiet -- "FreePascal ALPN runtime matrix passed" "$TEST_FILE" || \
  fail "ALPN runtime integration should emit a matrix-level success summary"

echo "[PASS] pure Pascal ALPN runtime matrix contract stays enforced"
