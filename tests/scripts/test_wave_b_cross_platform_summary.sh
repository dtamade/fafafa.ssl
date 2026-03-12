#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# 在项目内创建临时测试目录
TEST_DIR="tmp/test_wave_b_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Wave B Cross-Platform Summary - TODO Closure Contract"

# 创建 fixture Linux summary
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

# 创建 fixture macOS summary
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

# 创建 fixture Windows summary
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

# 创建 fixture Android summary
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

# 创建 fixture Linux examples JSON
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

# 运行脚本生成跨平台摘要（使用相对路径）
cd "$PROJECT_ROOT"

bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id test_contract \
  --linux-summary "$TEST_DIR/linux_summary.md" \
  --linux-examples "$TEST_DIR/examples.json" \
  --macos-summary "$TEST_DIR/macos_summary.md" \
  --windows-summary "$TEST_DIR/windows_summary.md" \
  --android-summary "$TEST_DIR/android_summary.md" \
  --output "$TEST_DIR/cross_platform_summary.md"

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/cross_platform_summary.md"

# 验证输出文件存在
if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] Output file not generated"
  exit 1
fi

# 检查 checklist 表格中是否有 TODO 占位符
TODO_COUNT=$(grep -c "TODO" "$OUTPUT_FILE" || true)

if [[ "$TODO_COUNT" -gt 0 ]]; then
  echo "[FAIL] Checklist contains $TODO_COUNT TODO placeholders when all platform summaries are provided"
  echo ""
  echo "Expected: No TODO in checklist when macOS and Windows summaries are provided"
  echo "Actual: Found $TODO_COUNT TODO entries"
  echo ""
  echo "Checklist section:"
  sed -n '/## 3) Cross-Platform Checklist/,/## 4)/p' "$OUTPUT_FILE"
  exit 1
fi

echo "[PASS] Checklist TODO closure contract passed - no TODO placeholders found"
exit 0
