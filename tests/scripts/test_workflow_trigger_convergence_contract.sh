#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CI_YML="$ROOT_DIR/.github/workflows/ci.yml"
PHASE_C_YML="$ROOT_DIR/.github/workflows/phase_c_tests.yml"
MATRIX_YML="$ROOT_DIR/.github/workflows/ci-matrix-draft.yml"
PLATFORMS_YML="$ROOT_DIR/.github/workflows/test-all-platforms.yml"

fail() {
  echo "[FAIL] $1"
  exit 1
}

has_key() {
  local file="$1"
  local key="$2"
  local normalized

  normalized="$(tr -d '\r' < "$file")"
  grep -Eq "^[[:space:]]*${key}:$" <<< "$normalized"
}

missing_key() {
  local file="$1"
  local key="$2"
  if has_key "$file" "$key"; then
    fail "$(basename "$file") should not declare ${key} trigger"
  fi
}

echo "[TEST] workflow trigger convergence contract"

has_key "$CI_YML" push || fail "ci.yml should keep push trigger"
has_key "$CI_YML" pull_request || fail "ci.yml should keep pull_request trigger"

has_key "$PHASE_C_YML" workflow_dispatch || fail "phase_c_tests.yml should be manual"
missing_key "$PHASE_C_YML" push
missing_key "$PHASE_C_YML" pull_request

has_key "$MATRIX_YML" workflow_dispatch || fail "ci-matrix-draft.yml should be manual"
missing_key "$MATRIX_YML" push
missing_key "$MATRIX_YML" pull_request

has_key "$PLATFORMS_YML" workflow_dispatch || fail "test-all-platforms.yml should keep manual dispatch"
has_key "$PLATFORMS_YML" schedule || fail "test-all-platforms.yml should keep nightly schedule"
missing_key "$PLATFORMS_YML" push
missing_key "$PLATFORMS_YML" pull_request

echo "[PASS] workflow trigger convergence contract passed"
