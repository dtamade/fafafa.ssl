#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TEST_FILE="tests/integration/test_freepascal_tls12_system_roots_runtime.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal TLS1.2 system-roots runtime contract"

rg -F --quiet -- "FAFAFA_TLS12_SYSTEM_ROOTS_HOSTS" "$TEST_FILE" || \
  fail "TLS1.2 system-roots integration should accept multi-host env override"

rg -F --quiet -- "WithTLS12" "$TEST_FILE" || \
  fail "TLS1.2 system-roots integration should build TLS1.2 client context"

rg -F --quiet -- "WithSystemRoots" "$TEST_FILE" || \
  fail "TLS1.2 system-roots integration should use system roots"

rg -F --quiet -- "Skip: no TLS1.2 system-roots hosts configured" "$TEST_FILE" || \
  fail "TLS1.2 system-roots integration should skip cleanly when hosts are not configured"

echo "[PASS] pure Pascal TLS1.2 system-roots runtime contract stays enforced"
