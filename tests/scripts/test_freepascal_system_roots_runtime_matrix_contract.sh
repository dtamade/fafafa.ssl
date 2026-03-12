#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TEST_FILE="tests/integration/test_freepascal_system_roots_runtime.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal system-roots runtime matrix contract"

rg -F --quiet -- "FAFAFA_SYSTEM_ROOTS_HOSTS" "$TEST_FILE" || \
  fail "system-roots runtime test should accept a multi-host env override"

rg -F --quiet -- "function ResolveTargetHosts: TStringList;" "$TEST_FILE" || \
  fail "system-roots runtime test should expose ResolveTargetHosts"

rg -F --quiet -- "ExtractStrings([',', ';', ' ', #9, #10, #13]" "$TEST_FILE" || \
  fail "system-roots runtime test should parse delimited host lists"

rg -F --quiet -- "if Result.Count = 0 then" "$TEST_FILE" || \
  fail "system-roots runtime test should only fall back to default hosts when no explicit host override is provided"

rg -F --quiet -- "www.google.com" "$TEST_FILE" || \
  fail "system-roots runtime test should keep google in the default host set"

rg -F --quiet -- "www.cloudflare.com" "$TEST_FILE" || \
  fail "system-roots runtime test should add a second default host candidate"

rg -F --quiet -- "www.github.com" "$TEST_FILE" || \
  fail "system-roots runtime test should add a third default host candidate"

rg -F --quiet -- "HostCandidatesToString" "$TEST_FILE" || \
  fail "system-roots runtime test should print host matrix summaries"

rg -F --quiet -- "for LIndex := 0 to LHosts.Count - 1 do" "$TEST_FILE" || \
  fail "system-roots runtime test should iterate across multiple hosts"

rg -F --quiet -- "FreePascal system-roots runtime matrix passed" "$TEST_FILE" || \
  fail "system-roots runtime test should emit a matrix-level success summary"

echo "[PASS] pure Pascal system-roots runtime matrix contract stays enforced"
