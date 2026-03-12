#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_b_android_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Wave B Cross-Platform Summary - Android Extension Contract"

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

cat > "$PROJECT_ROOT/$TEST_DIR/macos_summary.md" <<'MACOS_EOF'
# Wave B macOS Gate Summary

- run_id: test_macos
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | all modules compiled |
| modules | **PASS** | all tests passed |
| examples | **PASS** | 71/75 compiled |
MACOS_EOF

cat > "$PROJECT_ROOT/$TEST_DIR/windows_summary.md" <<'WINDOWS_EOF'
# Wave B Windows Gate Summary

- run_id: test_windows
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | all modules compiled |
| modules | **PASS** | all tests passed |
| examples | **PASS** | 71/75 compiled |
WINDOWS_EOF

cat > "$PROJECT_ROOT/$TEST_DIR/android_summary.md" <<'ANDROID_EOF'
# Wave B Android Gate Summary

- run_id: test_android
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | all modules compiled |
| modules | **PASS** | all tests passed |
| examples | **PASS** | 71/75 compiled |
ANDROID_EOF

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
  --run-id test_android_contract \
  --linux-summary "$TEST_DIR/linux_summary.md" \
  --linux-examples "$TEST_DIR/examples.json" \
  --macos-summary "$TEST_DIR/macos_summary.md" \
  --windows-summary "$TEST_DIR/windows_summary.md" \
  --android-summary "$TEST_DIR/android_summary.md" \
  --output "$TEST_DIR/cross_platform_summary.md"

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/cross_platform_summary.md"
[[ -f "$OUTPUT_FILE" ]] || { echo "[FAIL] output file not generated"; exit 1; }

TODO_COUNT=$(grep -c "TODO" "$OUTPUT_FILE" || true)
if [[ "$TODO_COUNT" -gt 0 ]]; then
  echo "[FAIL] checklist contains TODO placeholders with full four-platform evidence"
  sed -n '/## 3) Cross-Platform Checklist/,/## 4)/p' "$OUTPUT_FILE"
  exit 1
fi

if ! grep -Fq "| android | PASS |" "$OUTPUT_FILE"; then
  echo "[FAIL] platform evidence status missing android PASS row"
  sed -n '/## 1) Platform Evidence Status/,/## 2)/p' "$OUTPUT_FILE"
  exit 1
fi

if ! grep -Fq "| overall | PASS | PASS | PASS | PASS |" "$OUTPUT_FILE"; then
  echo "[FAIL] overall checklist should include android PASS column"
  sed -n '/## 3) Cross-Platform Checklist/,/## 4)/p' "$OUTPUT_FILE"
  exit 1
fi

echo "[PASS] Android extension contract passed"
