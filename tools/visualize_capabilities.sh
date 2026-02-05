#!/bin/bash
# 能力矩阵可视化工具启动脚本

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "=================================="
echo "fafafa.ssl 能力矩阵可视化工具"
echo "=================================="
echo

# 检查是否已编译测试程序
TEST_PROG="$PROJECT_ROOT/tests/test_capability_serialization"
if [ ! -f "$TEST_PROG" ]; then
    echo "编译测试程序..."
    cd "$PROJECT_ROOT"
    fpc -B -Fu./src tests/test_capability_serialization.pas > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "错误: 编译测试程序失败"
        echo "请手动运行: fpc -B -Fu./src tests/test_capability_serialization.pas"
        exit 1
    fi
    echo "✓ 测试程序编译完成"
fi

# 运行测试程序生成 JSON 文件
echo "生成后端能力 JSON 文件..."
cd "$PROJECT_ROOT"
"$TEST_PROG" > /dev/null 2>&1
echo "✓ JSON 文件生成完成"
echo

# 列出生成的文件
echo "生成的文件:"
for file in capability_*.json; do
    if [ -f "$file" ]; then
        echo "  - $file"
    fi
done
echo

# 打开可视化工具
HTML_FILE="$PROJECT_ROOT/tools/capability_visualizer.html"
if [ -f "$HTML_FILE" ]; then
    echo "正在打开可视化工具..."
    echo "文件: $HTML_FILE"
    echo

    # 尝试使用不同的浏览器
    if command -v xdg-open > /dev/null; then
        xdg-open "$HTML_FILE" 2>/dev/null &
        echo "✓ 已在默认浏览器中打开"
    elif command -v open > /dev/null; then
        open "$HTML_FILE" 2>/dev/null &
        echo "✓ 已在默认浏览器中打开"
    elif command -v firefox > /dev/null; then
        firefox "$HTML_FILE" 2>/dev/null &
        echo "✓ 已在 Firefox 中打开"
    elif command -v chrome > /dev/null; then
        chrome "$HTML_FILE" 2>/dev/null &
        echo "✓ 已在 Chrome 中打开"
    else
        echo "提示: 请手动在浏览器中打开以下文件:"
        echo "  file://$HTML_FILE"
    fi
else
    echo "错误: 找不到可视化工具文件"
    echo "  $HTML_FILE"
    exit 1
fi

echo
echo "=================================="
echo "提示:"
echo "1. 在浏览器中点击 '加载示例数据' 查看示例"
echo "2. 或点击 '选择文件' 加载生成的 JSON 文件"
echo "3. JSON 文件位于: $PROJECT_ROOT/"
echo "=================================="
