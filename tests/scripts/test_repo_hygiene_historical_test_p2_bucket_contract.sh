#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ARCHIVE_DIR="$ROOT_DIR/docs/archive/reports/test-p2-history"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical test_p2 bucket contract"

if git -C "$ROOT_DIR" ls-files "test-reports/test_p2_*" | grep -q .; then
  echo "[INFO] remaining tracked test_p2 bucket sample:"
  git -C "$ROOT_DIR" ls-files "test-reports/test_p2_*" | sed -n "1,20p"
  fail "historical test_p2_* bucket should not remain tracked under test-reports"
fi

[[ -d "$ARCHIVE_DIR" ]] || fail "missing archive dir: docs/archive/reports/test-p2-history"
[[ -f "$MANIFEST" ]] || fail "missing migration manifest"

expected_refs=(
  "test_p2_store_comprehensive_result.txt"
  "test_p2_ocsp_comprehensive_result.txt"
  "test_p2_ts_comprehensive_result.txt"
)

for name in "${expected_refs[@]}"; do
  [[ -f "$ARCHIVE_DIR/$name" ]] || fail "missing archived referenced P2 result: $name"
done

if rg -n 'test-reports/test_p2_[A-Za-z0-9_]+\.txt' "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_test_p2_refs.txt 2>/dev/null; then
  echo "[INFO] stale P2 refs outside archive:"
  sed -n "1,120p" /tmp/fafafa_historical_test_p2_refs.txt
  fail "stale test-reports/test_p2_* references should be migrated to archive paths"
fi

echo "[PASS] repo hygiene historical test_p2 bucket contract passed"
