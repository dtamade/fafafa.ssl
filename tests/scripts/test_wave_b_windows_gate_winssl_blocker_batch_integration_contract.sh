#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_windows_gate.ps1"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local pattern="$1"
  if ! rg -F --quiet -- "$pattern" "$SCRIPT"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,280p' "$SCRIPT" || true
    exit 1
  fi
}

echo "[TEST] wave b windows gate winssl blocker batch integration contract"

assert_contains '[switch]$SkipWinsslBlockerBatch'
assert_contains 'run_windows_winssl_blocker_batch_draft.sh'
assert_contains '--run-id $RunId'
assert_contains '--reports-dir $OutputDir'
assert_contains '--output $OutputDir/winssl_blocker_batch_${RunId}.md'
assert_contains '--strict'
assert_contains '$winsslBlockerExit = "SKIP"'
assert_contains '$winsslBlockerStatus = "SKIPPED"'
assert_contains '| winssl_blocker_batch | $winsslBlockerExit | $winsslBlockerStatus |'
assert_contains '$winsslBlockerStatus -eq "PASS" -or $winsslBlockerStatus -eq "SKIPPED"'

echo "[PASS] wave b windows gate winssl blocker batch integration contract passed"
