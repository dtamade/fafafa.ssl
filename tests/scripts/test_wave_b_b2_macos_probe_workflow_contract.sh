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

require_match 'if \[\[ -f "test-reports/wave_b_macos_gate_summary_\$\{RUN_ID\}\.md" \]\]; then[\s\S]*MACOS_CROSS_ARGS=\(--macos-summary "test-reports/wave_b_macos_gate_summary_\$\{RUN_ID\}\.md"\)[\s\S]*MACOS_SUMMARY_ARGS=\(--macos-summary "test-reports/wave_b_macos_gate_summary_\$\{RUN_ID\}\.md"\)[\s\S]*elif \[\[ -f "test-reports/wave_b_macos_gate_probe_\$\{RUN_ID\}\.json" \]\]; then[\s\S]*MACOS_CROSS_ARGS=\(--macos-probe "test-reports/wave_b_macos_gate_probe_\$\{RUN_ID\}\.json"\)' \
  'summary workflow should fall back to macOS probe evidence when no macOS summary is present'

require_match 'generate_wave_b_cross_platform_summary\.sh[\s\S]*"\$\{MACOS_CROSS_ARGS\[@\]\}"' \
  'summary workflow should pass macOS summary/probe arguments into cross-platform summary generation'

require_match 'check_wave_b_b2_closure_readiness\.sh[\s\S]*"\$\{MACOS_SUMMARY_ARGS\[@\]\}"' \
  'summary workflow should keep macOS summary-only arguments away from closure readiness when only a probe exists'

require_match 'check_wave_b_b2_evidence_consistency\.sh[\s\S]*"\$\{MACOS_CONSISTENCY_ARGS\[@\]\}"' \
  'summary workflow should pass macOS probe evidence into consistency checks when probe-only evidence is active'

echo "[PASS] wave-b-b2 macOS probe workflow contract passed"
