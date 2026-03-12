#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_b_missing_state_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

extract_platform_state() {
  local file="$1"
  local platform="$2"
  awk -F'|' -v target="$platform" '
    {
      if (NF >= 4) {
        c1 = $2
        c2 = $3
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", c1)
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", c2)
        if (c1 == target) {
          print c2
          exit
        }
      }
    }
  ' "$file"
}

extract_check_state() {
  local file="$1"
  local check_name="$2"
  local platform_col="$3"
  awk -F'|' -v target="$check_name" -v col="$platform_col" '
    {
      if (NF >= 6) {
        check_col = $2
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", check_col)
        if (check_col != target) {
          next
        }

        val = ""
        if (col == "linux") val = $3
        else if (col == "macos") val = $4
        else if (col == "windows") val = $5
        else if (col == "android") val = $6

        gsub(/^[[:space:]]+|[[:space:]]+$/, "", val)
        print val
        exit
      }
    }
  ' "$file"
}

echo "[TEST] wave-b cross-platform summary missing state contract"

cat > "$PROJECT_ROOT/$TEST_DIR/linux_summary.md" <<'LINUX_EOF'
# Wave B CI Gate Summary

- run_id: test_linux_missing
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
bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id test_missing_state \
  --linux-summary "$TEST_DIR/linux_summary.md" \
  --linux-examples "$TEST_DIR/examples.json" \
  --output "$TEST_DIR/cross_platform_summary.md"

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/cross_platform_summary.md"
[[ -f "$OUTPUT_FILE" ]] || fail "cross-platform summary should be generated"

macos_state="$(extract_platform_state "$OUTPUT_FILE" "macos")"
windows_state="$(extract_platform_state "$OUTPUT_FILE" "windows")"
android_state="$(extract_platform_state "$OUTPUT_FILE" "android")"

[[ "$macos_state" == "MISSING" ]] || fail "macos platform state should be MISSING, got: ${macos_state:-<empty>}"
[[ "$windows_state" == "MISSING" ]] || fail "windows platform state should be MISSING, got: ${windows_state:-<empty>}"
[[ "$android_state" == "MISSING" ]] || fail "android platform state should be MISSING, got: ${android_state:-<empty>}"

for check in compile_all_modules p2_modules_gate examples_compile_gate overall; do
  for platform in macos windows android; do
    value="$(extract_check_state "$OUTPUT_FILE" "$check" "$platform")"
    [[ "$value" == "MISSING" ]] || fail "$platform $check should be MISSING, got: ${value:-<empty>}"
  done
done

TODO_COUNT=$(grep -c "TODO" "$OUTPUT_FILE" || true)
[[ "$TODO_COUNT" -eq 0 ]] || fail "summary should not expose TODO placeholders, found: $TODO_COUNT"

echo "[PASS] wave-b cross-platform summary missing state contract passed"
