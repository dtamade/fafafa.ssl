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
    sed -n '1,320p' "$SCRIPT" || true
    exit 1
  fi
}

echo "[TEST] wave b windows gate dry-run observability contract"

assert_contains 'Write-Host "[DRY-RUN] run_id=$RunId"'
assert_contains 'Write-Host "[DRY-RUN] output_dir=$OutputDir"'
assert_contains 'Write-Host "[DRY-RUN] summary=$SummaryRel"'
assert_contains 'Write-Host "[DRY-RUN] winssl_blocker_log=$WinsslBlockerLogRel"'
assert_contains 'Write-Host "[DRY-RUN] winssl_log=$WinsslLogRel"'
assert_contains 'Write-Host "[DRY-RUN] openssl_log=$OpenSSLLogRel"'
assert_contains 'Write-Host "[DRY-RUN] modules_log=$ModulesLogRel"'

echo "[PASS] wave b windows gate dry-run observability contract passed"
