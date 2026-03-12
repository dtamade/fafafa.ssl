#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/revalidate_closure_gate_after_autofix_draft.sh"
REL_WORK="tmp/test_revalidate_closure_gate_strict"
REL_OUTPUT="$REL_WORK/revalidate_strict.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    echo "[INFO] top of output ($file):"
    sed -n '1,120p' "$file" || true
    exit 1
  fi
}

run_strict_contract() {
  local missing_script_rel="$REL_WORK/missing_closure_gate.sh"

  rm -rf "$ROOT_DIR/$REL_WORK"
  mkdir -p "$ROOT_DIR/$REL_WORK"
  rm -f "$ROOT_DIR/$REL_OUTPUT" "/tmp/$REL_OUTPUT"

  if (cd /tmp && bash "$SCRIPT" \
    --revalidate-id strict_contract_missing_script \
    --closure-gate-script "$missing_script_rel" \
    --output "$REL_OUTPUT" \
    --strict >/dev/null 2>&1); then
    fail "strict mode should fail when revalidation_status is script-not-found"
  fi

  [[ -f "$ROOT_DIR/$REL_OUTPUT" ]] || fail "strict mode should still write report under project root"
  [[ ! -f "/tmp/$REL_OUTPUT" ]] || fail "strict mode output leaked into /tmp"

  assert_contains "$ROOT_DIR/$REL_OUTPUT" "| revalidation_status | script-not-found |"
  assert_contains "$ROOT_DIR/$REL_OUTPUT" "| overall_status | pending |"

  echo "[PASS] strict mode contract passed"
}

run_strict_contract
