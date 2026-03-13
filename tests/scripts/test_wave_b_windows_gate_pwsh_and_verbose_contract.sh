#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave_b windows gate pwsh + verbose contract"

for f in run_openssl_tests.ps1 run_winssl_tests.ps1; do
  path="$ROOT_DIR/$f"
  if [[ ! -f "$path" ]]; then
    fail "missing file: $f"
  fi

  if ! rg -F --quiet -- '[CmdletBinding()]' "$path"; then
    fail "$f should use [CmdletBinding()]"
  fi

  if rg -F --quiet -- '[switch]$Verbose' "$path"; then
    fail "$f must not declare [switch]\$Verbose (conflicts with common -Verbose)"
  fi

  if ! rg -F --quiet -- '$PSBoundParameters' "$path"; then
    fail "$f should use \$PSBoundParameters to detect common -Verbose"
  fi

  if ! rg -F --quiet -- "ContainsKey('Verbose')" "$path"; then
    fail "$f should gate verbose output via ContainsKey('Verbose')"
  fi
done

win_gate="$ROOT_DIR/scripts/run_wave_b_windows_gate.ps1"
if [[ ! -f "$win_gate" ]]; then
  fail "missing file: scripts/run_wave_b_windows_gate.ps1"
fi

if ! rg -n --quiet "Get-Command pwsh" "$win_gate"; then
  fail "run_wave_b_windows_gate should prefer pwsh when available"
fi

if ! rg -F --quiet -- 'Out-File -FilePath $LogPath -Encoding utf8' "$win_gate"; then
  fail "run_wave_b_windows_gate should write logs as utf8 for artifact readability"
fi

echo "[PASS] wave_b windows gate pwsh + verbose contract passed"
