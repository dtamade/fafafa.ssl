#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RUN_ID="test_default_output"
TEST_DIR="tmp/test_wave_b_default_output_$$"
OUT_FILE="$PROJECT_ROOT/tmp/wave_b_reports/wave_b_cross_platform_summary_${RUN_ID}.md"
LEGACY_OUT="$PROJECT_ROOT/test-reports/wave_b_cross_platform_summary_${RUN_ID}.md"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR" "$OUT_FILE" "$LEGACY_OUT"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave_b cross-platform summary default output contract"

rm -rf "$PROJECT_ROOT/$TEST_DIR" "$OUT_FILE" "$LEGACY_OUT"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"

cat > "$PROJECT_ROOT/$TEST_DIR/linux_summary.md" <<'LINUX_EOF'
# Wave B CI Gate Summary

- run_id: test_linux
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | 157/157 |
| run_all_module_tests | **PASS** | 100% |
| verify_examples_compile | **PASS** | 71/75 |
LINUX_EOF

cat > "$PROJECT_ROOT/$TEST_DIR/examples.json" <<'JSON_EOF'
{
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
JSON_EOF

cd "$PROJECT_ROOT"
bash scripts/generate_wave_b_cross_platform_summary.sh   --run-id "$RUN_ID"   --linux-summary "$TEST_DIR/linux_summary.md"   --linux-examples "$TEST_DIR/examples.json"

[[ -f "$OUT_FILE" ]] || fail "default output should be written under tmp/"
[[ ! -f "$LEGACY_OUT" ]] || fail "default output should no longer be written under test-reports/"

if ! rg -F --quiet 'Wave B Cross-Platform Summary' "$OUT_FILE"; then
  sed -n '1,200p' "$OUT_FILE" || true
  fail "output file should contain generated summary"
fi

echo "[PASS] wave_b cross-platform summary default output contract passed"
