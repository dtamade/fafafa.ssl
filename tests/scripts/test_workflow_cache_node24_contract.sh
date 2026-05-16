#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOWS_DIR="$ROOT_DIR/.github/workflows"

fail() {
  echo "[FAIL] $1"
  exit 1
}

pass() {
  echo "[PASS] $1"
}

[[ -d "$WORKFLOWS_DIR" ]] || fail "missing workflows directory: .github/workflows"

mapfile -t workflow_files < <(find "$WORKFLOWS_DIR" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yml.disabled' \) | sort)
[[ "${#workflow_files[@]}" -gt 0 ]] || fail "no workflow files found under .github/workflows"

for workflow in "${workflow_files[@]}"; do
  if rg -n 'uses:\s*actions/cache@v[1-4]\b' "$workflow" >/dev/null; then
    rel="${workflow#$ROOT_DIR/}"
    fail "$rel must not keep pre-Node24-default actions/cache references"
  fi
done
pass "all workflow files avoid actions/cache@v1 through @v4"

required_v5_workflows=(
  ".github/workflows/test-all-platforms.yml.disabled"
  ".github/workflows/winssl-tests.yml.disabled"
)

for rel in "${required_v5_workflows[@]}"; do
  abs="$ROOT_DIR/$rel"
  [[ -f "$abs" ]] || fail "missing expected workflow: $rel"
  if rg -n 'uses:\s*actions/cache@v5\b' "$abs" >/dev/null; then
    pass "$rel uses actions/cache@v5"
  else
    fail "$rel must use actions/cache@v5"
  fi
done

echo "[PASS] workflow cache Node24 contract passed"
