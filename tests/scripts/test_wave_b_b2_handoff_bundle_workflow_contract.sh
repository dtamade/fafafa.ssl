#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOWS=(
  "$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml"
  "$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml.disabled"
)

fail() {
  echo "[FAIL] $1"
  exit 1
}

require_match() {
  local workflow="$1"
  local label="$2"
  local pattern="$3"
  local message="$4"
  if ! rg -n --multiline --multiline-dotall "$pattern" "$workflow" >/dev/null; then
    fail "$label: $message"
  fi
}

require_no_match() {
  local workflow="$1"
  local label="$2"
  local pattern="$3"
  local message="$4"
  if rg -n --multiline --multiline-dotall "$pattern" "$workflow" >/dev/null; then
    fail "$label: $message"
  fi
}

echo "[TEST] wave-b-b2 handoff bundle workflow contract"

for workflow in "${WORKFLOWS[@]}"; do
  if [[ ! -f "$workflow" ]]; then
    fail "missing workflow: $workflow"
  fi

  label="$(basename "$workflow")"

  require_match "$workflow" "$label" 'PREPARE_ARGS=\([\s\S]*--run-id "\$RUN_ID"[\s\S]*--linux-summary "\$LINUX_SUMMARY"[\s\S]*--linux-examples "\$LINUX_EXAMPLES"[\s\S]*--output-dir "test-reports"[\s\S]*\)' \
    'summary workflow should build prepare arguments from run id, linux summary, linux examples, and output dir'

  require_match "$workflow" "$label" 'if \[\[ "\$\{\{ github\.event\.inputs\.strict_closure \}\}" == "true" \]\]; then[\s\S]*PREPARE_ARGS\+=\(--strict\)' \
    'summary workflow should map strict_closure=true to prepare --strict'

  require_match "$workflow" "$label" 'bash scripts/prepare_wave_b_b2_handoff_bundle\.sh[\s\S]*"\$\{PREPARE_ARGS\[@\]\}"' \
    'summary workflow should generate summary artifacts through prepare_wave_b_b2_handoff_bundle.sh'

  require_match "$workflow" "$label" 'test-reports/wave_b_b2_handoff_bundle_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.md' \
    'summary workflow should upload the generated handoff bundle artifact'

  require_no_match "$workflow" "$label" 'MACOS_CROSS_ARGS=\(' \
    'summary workflow should not duplicate macOS cross-summary argument selection after switching to prepare'

  require_no_match "$workflow" "$label" 'MACOS_CONSISTENCY_ARGS=\(' \
    'summary workflow should not duplicate macOS consistency argument selection after switching to prepare'

  require_no_match "$workflow" "$label" 'WINDOWS_EVIDENCE_ARGS=\(' \
    'summary workflow should not duplicate Windows runtime artifact argument selection after switching to prepare'

  require_no_match "$workflow" "$label" 'bash scripts/generate_wave_b_cross_platform_summary\.sh' \
    'summary workflow should not call cross-platform summary directly after switching to prepare'

  require_no_match "$workflow" "$label" 'bash scripts/check_wave_b_b2_closure_readiness\.sh' \
    'summary workflow should not call closure readiness directly after switching to prepare'

  require_no_match "$workflow" "$label" 'bash scripts/check_wave_b_b2_evidence_consistency\.sh' \
    'summary workflow should not call evidence consistency directly after switching to prepare'
done

echo "[PASS] wave-b-b2 handoff bundle workflow contract passed"
