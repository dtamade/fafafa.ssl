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

echo "[TEST] wave-b-b2 linux baseline required workflow contract"

for workflow in "${WORKFLOWS[@]}"; do
  if [[ ! -f "$workflow" ]]; then
    fail "missing workflow: $workflow"
  fi

  label="$(basename "$workflow")"

  require_no_match "$workflow" "$label" 'run_linux_baseline:' \
    'workflow_dispatch should not expose run_linux_baseline after Linux summary became a required handoff truth source'

  require_no_match "$workflow" "$label" "if: \\$\\{\\{ github\\.event\\.inputs\\.run_linux_baseline != 'false' \\}\\}" \
    'workflow should not keep conditional Linux gate or Linux artifact download branches after Linux baseline became required'

  require_match "$workflow" "$label" 'linux-gate:\s*\n\s*needs: setup\s*\n\s*runs-on: ubuntu-latest' \
    'workflow should run the Linux gate unconditionally'

  require_match "$workflow" "$label" 'Download Linux evidence[\s\S]*uses: actions/download-artifact@' \
    'summary workflow should always download Linux evidence for the required handoff truth path'
done

echo "[PASS] wave-b-b2 linux baseline required workflow contract passed"
