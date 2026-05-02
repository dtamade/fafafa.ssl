#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_b_no_todo_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/linux_summary.md" <<'LINUX_EOF'
# Wave B CI Gate Summary

- run_id: test_linux_pending
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
  --run-id test_pending_contract \
  --linux-summary "$TEST_DIR/linux_summary.md" \
  --linux-examples "$TEST_DIR/examples.json" \
  --output "$TEST_DIR/cross_platform_summary.md"

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/cross_platform_summary.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] output file not generated"
  exit 1
fi

TODO_COUNT=$(grep -c "TODO" "$OUTPUT_FILE" || true)
if [[ "$TODO_COUNT" -gt 0 ]]; then
  echo "[FAIL] summary still contains TODO placeholders when non-Linux evidence is pending"
  sed -n '/## 3) Cross-Platform Checklist/,/## 4)/p' "$OUTPUT_FILE"
  exit 1
fi

macos_compile=$(grep "compile_all_modules" "$OUTPUT_FILE" | awk -F'|' '{print $4}' | tr -d ' ')
windows_compile=$(grep "compile_all_modules" "$OUTPUT_FILE" | awk -F'|' '{print $5}' | tr -d ' ')

if [[ "$macos_compile" != "PENDING" || "$windows_compile" != "PENDING" ]]; then
  echo "[FAIL] missing platform evidence should map to PENDING"
  echo "[INFO] macos_compile=$macos_compile windows_compile=$windows_compile"
  sed -n '/## 3) Cross-Platform Checklist/,/## 4)/p' "$OUTPUT_FILE"
  exit 1
fi

echo "[PASS] pending-evidence summary uses stable states instead of TODO placeholders"
