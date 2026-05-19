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
  if ! rg -n --multiline --multiline-dotall -- "$pattern" "$workflow" >/dev/null; then
    fail "$label: $message"
  fi
}

require_no_match() {
  local workflow="$1"
  local label="$2"
  local pattern="$3"
  local message="$4"
  if rg -n --multiline --multiline-dotall -- "$pattern" "$workflow" >/dev/null; then
    fail "$label: $message"
  fi
}

echo "[TEST] wave-b-b2 optional runner artifact download workflow contract"

for workflow in "${WORKFLOWS[@]}"; do
  if [[ ! -f "$workflow" ]]; then
    fail "missing workflow: $workflow"
  fi

  label="$(basename "$workflow")"

  require_match "$workflow" "$label" 'Download Linux evidence[\s\S]*uses: actions/download-artifact@' \
    'workflow should keep the Linux artifact download step for required summary truth'

  require_no_match "$workflow" "$label" '- name: Download Linux evidence\s*\n\s*continue-on-error: true' \
    'workflow should not soften missing Linux artifacts because Linux summary remains required'

  require_match "$workflow" "$label" 'Download macOS evidence[\s\S]*continue-on-error: true[\s\S]*uses: actions/download-artifact@' \
    'workflow should tolerate missing macOS artifacts so prepare can render missing-evidence handoff truth'

  require_match "$workflow" "$label" 'Download Windows evidence[\s\S]*continue-on-error: true[\s\S]*uses: actions/download-artifact@' \
    'workflow should tolerate missing Windows artifacts so prepare can render missing-evidence handoff truth'
done

echo "[PASS] wave-b-b2 optional runner artifact download workflow contract passed"
