#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOW="$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml"

fail() {
  echo "[FAIL] $1"
  exit 1
}

require_match() {
  local pattern="$1"
  local message="$2"
  if ! rg -n --multiline --multiline-dotall "$pattern" "$WORKFLOW" >/dev/null; then
    fail "$message"
  fi
}

echo "[TEST] wave-b-b2 windows runtime workflow contract"

if [[ ! -f "$WORKFLOW" ]]; then
  fail "missing workflow: .github/workflows/wave-b-b2-manual.yml"
fi

require_match 'runs-on:\s*windows-latest' \
  'workflow should keep a windows-latest gate'

require_match 'choco install -y .*lazarus|choco install -y lazarus.*' \
  'windows workflow should install Lazarus for lazbuild-based runtime checks'

require_match 'Get-Command lazbuild' \
  'windows workflow should verify lazbuild availability'

require_match 'quick_winssl_validation\.ps1' \
  'windows workflow should run quick WinSSL smoke before Wave B gate'

require_match 'run_wave_b_windows_gate\.ps1' \
  'windows workflow should run the Wave B Windows gate'

require_match 'Start-Transcript -Path .*winssl_runtime_suite_' \
  'windows workflow should capture broader WinSSL suite transcript'

require_match 'tests/run_winssl_tests\.ps1' \
  'windows workflow should run the broader WinSSL suite'

require_match 'wave_b_windows_gate_summary_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.md' \
  'windows workflow should upload Wave B Windows gate summary artifact'

require_match 'winssl_runtime_suite_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.log' \
  'windows workflow should upload broader WinSSL suite transcript artifact'

require_match 'prepare_wave_b_b2_handoff_bundle\.sh' \
  'summary workflow should route Windows runtime evidence through prepare_wave_b_b2_handoff_bundle.sh as the single handoff truth source'

echo "[PASS] wave-b-b2 windows runtime workflow contract passed"
