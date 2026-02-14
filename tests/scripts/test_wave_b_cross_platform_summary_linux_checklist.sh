#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# 在项目内创建临时测试目录
TEST_DIR="tmp/test_wave_b_linux_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Wave B Cross-Platform Summary - Linux Checklist Status Mapping"

# 创建 fixture Linux summary with mixed statuses
cat > "$PROJECT_ROOT/$TEST_DIR/linux_summary.md" <<'LINUX_EOF'
# Wave B CI Gate Summary

- run_id: test_linux_mixed
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **SKIP** | skipped due to env |
| run_all_module_tests | **FAIL** | 1 test failed |
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

# 运行脚本生成跨平台摘要
cd "$PROJECT_ROOT"

bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id test_linux_checklist \
  --linux-summary "$TEST_DIR/linux_summary.md" \
  --linux-examples "$TEST_DIR/examples.json" \
  --macos-summary "$TEST_DIR/macos_summary.md" \
  --output "$TEST_DIR/cross_platform_summary.md"

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/cross_platform_summary.md"

# 验证输出文件存在
if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] Output file not generated"
  exit 1
fi

# 提取 Linux checklist 列的状态
LINUX_COMPILE=$(grep "compile_all_modules" "$OUTPUT_FILE" | awk -F'|' '{print $3}' | tr -d ' ')
LINUX_MODULES=$(grep "p2_modules_gate" "$OUTPUT_FILE" | awk -F'|' '{print $3}' | tr -d ' ')
LINUX_EXAMPLES=$(grep "examples_compile_gate" "$OUTPUT_FILE" | awk -F'|' '{print $3}' | tr -d ' ')

echo "[DEBUG] Linux checklist statuses:"
echo "  compile_all_modules: $LINUX_COMPILE (expected: SKIP)"
echo "  p2_modules_gate: $LINUX_MODULES (expected: FAIL)"
echo "  examples_compile_gate: $LINUX_EXAMPLES (expected: PASS)"

# 验证状态映射
FAILED=0

if [[ "$LINUX_COMPILE" != "SKIP" ]]; then
  echo "[FAIL] Linux compile status should be SKIP, got: $LINUX_COMPILE"
  FAILED=1
fi

if [[ "$LINUX_MODULES" != "FAIL" ]]; then
  echo "[FAIL] Linux modules status should be FAIL, got: $LINUX_MODULES"
  FAILED=1
fi

if [[ "$LINUX_EXAMPLES" != "PASS" ]]; then
  echo "[FAIL] Linux examples status should be PASS, got: $LINUX_EXAMPLES"
  FAILED=1
fi

if [[ $FAILED -eq 1 ]]; then
  echo ""
  echo "Expected: Linux checklist reflects actual step statuses (SKIP/FAIL/PASS)"
  echo "Actual: Linux checklist does not match fixture statuses"
  exit 1
fi

echo "[PASS] Linux checklist status mapping contract passed"
exit 0
