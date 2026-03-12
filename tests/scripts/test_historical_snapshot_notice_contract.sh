#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] historical snapshot notice contract"

files=(
  "docs/testing/README_TESTING.md"
  "docs/testing/TESTING.md"
  "docs/testing/TEST_COVERAGE_ASSESSMENT.md"
  "docs/testing/TEST_PLAN.md"
  "docs/testing/TEST_RESULTS.md"
  "docs/validation/validation_report_20251003_013646.md"
)

for rel in "${files[@]}"; do
  path="$ROOT_DIR/$rel"
  [[ -f "$path" ]] || fail "missing file: $rel"

  grep -Fq 'Historical snapshot' "$path" || fail "$rel should include a Historical snapshot notice"
  grep -Fq 'docs/testing/TESTING_README.md' "$path" || fail "$rel should point to docs/testing/TESTING_README.md"
done

echo "[PASS] historical snapshot notice contract passed"
