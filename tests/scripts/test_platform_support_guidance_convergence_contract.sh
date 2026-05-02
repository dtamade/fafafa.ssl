#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

FILE="docs/PLATFORM_SUPPORT.md"

assert_contains() {
  local file="$1"
  local pattern="$2"
  local message="$3"

  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] $message"
    echo "[INFO] top of $file:"
    sed -n '1,320p' "$file" || true
    exit 1
  fi
}

assert_not_contains() {
  local file="$1"
  local pattern="$2"
  local message="$3"

  if rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] $message"
    rg -n -F -- "$pattern" "$file" || true
    exit 1
  fi
}

assert_contains "$FILE" "## 当前工程验证入口" \
  "Platform support doc is missing the current engineering entry section"
assert_contains "$FILE" "docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md" \
  "Platform support doc is missing the canonical Wave C closeout entrypoint"
assert_contains "$FILE" "docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md" \
  "Platform support doc is missing the current Wave C chain entrypoint"
assert_contains "$FILE" "python3 scripts/compile_all_modules.py" \
  "Platform support doc is missing the Linux canonical compile command"
assert_contains "$FILE" "bash scripts/run_minimal_ci_gate.sh --fast-local" \
  "Platform support doc is missing the Linux canonical minimal gate command"
assert_contains "$FILE" "tests/openssl/test_openssl_simple.pas" \
  "Platform support doc is missing the macOS focused smoke source path"
assert_contains "$FILE" "run_core_tests.ps1" \
  "Platform support doc no longer points Windows guidance at the real PowerShell test script"
assert_contains "$FILE" "run_winssl_tests.ps1" \
  "Platform support doc no longer points Windows guidance at the real WinSSL PowerShell script"

assert_not_contains "$FILE" "build_linux.sh" \
  "Platform support doc still treats build_linux.sh as default guidance"
assert_not_contains "$FILE" "run_core_tests.sh" \
  "Platform support doc still references the removed shell core-test script"
assert_not_contains "$FILE" "build_macos.sh" \
  "Platform support doc still treats build_macos.sh as default guidance"

echo "[PASS] platform support doc stays aligned with current platform guidance"
