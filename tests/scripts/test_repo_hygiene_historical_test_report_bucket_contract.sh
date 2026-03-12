#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ARCHIVE_DIR="$ROOT_DIR/docs/archive/reports/test-report-history"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical test-report bucket contract"

if git -C "$ROOT_DIR" ls-files "test-reports/test_report_*.txt" | grep -q .; then
  echo "[INFO] remaining tracked test_report bucket sample:"
  git -C "$ROOT_DIR" ls-files "test-reports/test_report_*.txt" | sed -n "1,20p"
  fail "historical test_report_*.txt bucket should not remain tracked under test-reports"
fi

[[ -d "$ARCHIVE_DIR" ]] || fail "missing archive dir: docs/archive/reports/test-report-history"
[[ -f "$MANIFEST" ]] || fail "missing migration manifest"

expected_refs=(
  "test_report_20260207_024318.txt"
  "test_report_20260207_025410.txt"
  "test_report_20260207_025418.txt"
  "test_report_20260208_011843.txt"
  "test_report_20260207_024905.txt"
  "test_report_20260207_022221.txt"
  "test_report_20260207_024912.txt"
  "test_report_20260212_162709.txt"
  "test_report_20260212_134110.txt"
  "test_report_20260213_002947.txt"
  "test_report_20260212_164946.txt"
  "test_report_20260212_204309.txt"
  "test_report_20260212_133118.txt"
  "test_report_20260212_165435.txt"
  "test_report_20260212_170709.txt"
  "test_report_20260212_144138.txt"
  "test_report_20260212_143429.txt"
  "test_report_20260212_161654.txt"
  "test_report_20260207_022236.txt"
  "test_report_20260212_171744.txt"
  "test_report_20260212_230410.txt"
  "test_report_20260212_194600.txt"
  "test_report_20260212_163708.txt"
  "test_report_20260212_135750.txt"
  "test_report_20260212_140825.txt"
  "test_report_20260212_154040.txt"
  "test_report_20260212_152746.txt"
  "test_report_20260212_181218.txt"
)

for name in "${expected_refs[@]}"; do
  [[ -f "$ARCHIVE_DIR/$name" ]] || fail "missing archived referenced report: $name"
done

if rg -n 'test-reports/test_report_[0-9_]+\.txt' "$ROOT_DIR" --glob '!test-reports/**' --glob '!docs/archive/reports/**' >/tmp/fafafa_historical_test_report_refs.txt 2>/dev/null; then
  echo "[INFO] stale refs outside archive:"
  sed -n "1,120p" /tmp/fafafa_historical_test_report_refs.txt
  fail "stale test-reports/test_report references should be migrated to archive paths"
fi

echo "[PASS] repo hygiene historical test-report bucket contract passed"
