#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_b_windows_blocker_layout_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

extract_windows_check() {
  local file="$1"
  local check_name="$2"
  awk -F'|' -v target="$check_name" '
    {
      if (NF >= 6) {
        check_col = $2
        windows_col = $5
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", check_col)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", windows_col)
        if (check_col == target) {
          print windows_col
          exit
        }
      }
    }
  ' "$file"
}

echo "[TEST] wave-b cross-platform summary windows blocker layout compatibility"

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

cat > "$PROJECT_ROOT/$TEST_DIR/windows_summary.md" <<'WINDOWS_EOF'
# Wave B Windows Gate Summary

- run_id: test_windows
- overall: PASS

## Steps

| step | exit | status | evidence |
|------|------|--------|----------|
| winssl_blocker_batch | 0 | PASS | test-reports/winssl_blocker.log |
| winssl | 0 | PASS | test-reports/winssl.log |
| openssl | 0 | PASS | test-reports/openssl.log |
| modules | 0 | PASS | test-reports/modules.log |
WINDOWS_EOF

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
bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id test_windows_blocker_layout \
  --linux-summary "$TEST_DIR/linux_summary.md" \
  --linux-examples "$TEST_DIR/examples.json" \
  --windows-summary "$TEST_DIR/windows_summary.md" \
  --output "$TEST_DIR/cross_platform_summary.md"

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/cross_platform_summary.md"
[[ -f "$OUTPUT_FILE" ]] || fail "cross-platform summary should be generated"

windows_compile="$(extract_windows_check "$OUTPUT_FILE" "compile_all_modules")"
windows_modules="$(extract_windows_check "$OUTPUT_FILE" "p2_modules_gate")"
windows_examples="$(extract_windows_check "$OUTPUT_FILE" "examples_compile_gate")"
windows_overall="$(extract_windows_check "$OUTPUT_FILE" "overall")"

[[ "$windows_compile" == "PASS" ]] || fail "windows compile_all_modules should be PASS, got: ${windows_compile:-<empty>}"
[[ "$windows_modules" == "PASS" ]] || fail "windows p2_modules_gate should be PASS, got: ${windows_modules:-<empty>}"
[[ "$windows_examples" == "PASS" ]] || fail "windows examples_compile_gate should be PASS, got: ${windows_examples:-<empty>}"
[[ "$windows_overall" == "PASS" ]] || fail "windows overall should be PASS, got: ${windows_overall:-<empty>}"

echo "[PASS] wave-b cross-platform summary windows blocker layout compatibility passed"
