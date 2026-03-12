#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
INDEX_DOC="$ROOT_DIR/docs/PLANS_CURRENT_INDEX.md"
DOCS_README="$ROOT_DIR/docs/README.md"
DOCS_INDEX="$ROOT_DIR/docs/DOCUMENTATION_INDEX.md"
PLANS_README="$ROOT_DIR/docs/plans/README.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,220p' "$file" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] plans current index contract"

[[ -f "$INDEX_DOC" ]] || fail "docs/PLANS_CURRENT_INDEX.md should exist"

assert_contains "$INDEX_DOC" "testing/CURRENT_HEALTH.md"
assert_contains "$INDEX_DOC" "testing/TESTING_README.md"
assert_contains "$INDEX_DOC" "plans/README.md"
assert_contains "$INDEX_DOC" "plans/2026-03-07-runtime-contracts-current-index.md"
assert_contains "$INDEX_DOC" "plans/2026-03-08-pkcs11-managed-result-warning-alignment.md"
assert_contains "$INDEX_DOC" "plans/2026-03-current-summary.md"

assert_contains "$DOCS_README" "PLANS_CURRENT_INDEX.md"
assert_contains "$DOCS_INDEX" "PLANS_CURRENT_INDEX.md"
assert_contains "$PLANS_README" "../PLANS_CURRENT_INDEX.md"
assert_contains "$PLANS_README" "2026-03-current-summary.md"

echo "[PASS] plans current index contract passed"
