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

require_no_match() {
  local pattern="$1"
  local message="$2"
  if rg -n --multiline --multiline-dotall "$pattern" "$WORKFLOW" >/dev/null; then
    fail "$message"
  fi
}

echo "[TEST] wave-b-b2 handoff bundle workflow contract"

if [[ ! -f "$WORKFLOW" ]]; then
  fail "missing workflow: .github/workflows/wave-b-b2-manual.yml"
fi

require_match 'PREPARE_ARGS=\([\s\S]*--run-id "\$RUN_ID"[\s\S]*--linux-summary "\$LINUX_SUMMARY"[\s\S]*--linux-examples "\$LINUX_EXAMPLES"[\s\S]*--output-dir "test-reports"[\s\S]*\)' \
  'summary workflow should build prepare arguments from run id, linux summary, linux examples, and output dir'

require_match 'if \[\[ "\$\{\{ github\.event\.inputs\.strict_closure \}\}" == "true" \]\]; then[\s\S]*PREPARE_ARGS\+=\(--strict\)' \
  'summary workflow should map strict_closure=true to prepare --strict'

require_match 'bash scripts/prepare_wave_b_b2_handoff_bundle\.sh[\s\S]*"\$\{PREPARE_ARGS\[@\]\}"' \
  'summary workflow should generate summary artifacts through prepare_wave_b_b2_handoff_bundle.sh'

require_match 'test-reports/wave_b_b2_handoff_bundle_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.md' \
  'summary workflow should upload the generated handoff bundle artifact'

require_no_match 'MACOS_CROSS_ARGS=\(' \
  'summary workflow should not duplicate macOS cross-summary argument selection after switching to prepare'

require_no_match 'MACOS_CONSISTENCY_ARGS=\(' \
  'summary workflow should not duplicate macOS consistency argument selection after switching to prepare'

require_no_match 'WINDOWS_EVIDENCE_ARGS=\(' \
  'summary workflow should not duplicate Windows runtime artifact argument selection after switching to prepare'

require_no_match 'bash scripts/generate_wave_b_cross_platform_summary\.sh' \
  'summary workflow should not call cross-platform summary directly after switching to prepare'

require_no_match 'bash scripts/check_wave_b_b2_closure_readiness\.sh' \
  'summary workflow should not call closure readiness directly after switching to prepare'

require_no_match 'bash scripts/check_wave_b_b2_evidence_consistency\.sh' \
  'summary workflow should not call evidence consistency directly after switching to prepare'

echo "[PASS] wave-b-b2 handoff bundle workflow contract passed"
