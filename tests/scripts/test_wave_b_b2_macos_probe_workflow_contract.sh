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

echo "[TEST] wave-b-b2 macOS probe workflow contract"

if [[ ! -f "$WORKFLOW" ]]; then
  fail "missing workflow: .github/workflows/wave-b-b2-manual.yml"
fi

require_match 'run_wave_b_macos_gate\.sh' \
  'workflow should keep the macOS Wave B gate entrypoint'

require_match 'wave_b_macos_gate_summary_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.md' \
  'workflow should upload the macOS gate summary artifact'

require_match 'wave_b_macos_gate_probe_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.json' \
  'workflow should upload the macOS probe artifact for probe-fallback handling'

require_match 'prepare_wave_b_b2_handoff_bundle\.sh' \
  'summary workflow should route macOS evidence through prepare_wave_b_b2_handoff_bundle.sh as the single handoff truth source'

echo "[PASS] wave-b-b2 macOS probe workflow contract passed"
