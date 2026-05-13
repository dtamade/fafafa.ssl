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

echo "[TEST] wave-b-b2 strict input description contract"

for workflow in "${WORKFLOWS[@]}"; do
  if [[ ! -f "$workflow" ]]; then
    fail "missing workflow: $workflow"
  fi

  label="$(basename "$workflow")"

  require_match "$workflow" "$label" 'strict_closure:' \
    'workflow should keep the compatibility input key strict_closure'

  require_no_match "$workflow" "$label" 'description: Fail workflow if B2 not closed' \
    'strict_closure description should no longer claim closure-only failure semantics'

  require_match "$workflow" "$label" 'description: Fail workflow if Wave B/B2 handoff is not fully closed and consistent' \
    'strict_closure description should explain the full handoff strict semantics'
done

echo "[PASS] wave-b-b2 strict input description contract passed"
