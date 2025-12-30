#!/bin/bash
# 重构为 fafafa.ssl.base.pas 方案

set -e

echo "=== 重构为 fafafa.ssl.base.pas ==="
echo

cd src

# 阶段 1: 清理废弃文件
echo "阶段 1: 清理废弃文件..."
rm -f *.bak *_new.pas *_old.pas *.o *.ppu
echo "✓ 清理完成"
echo

# 阶段 2: 创建 base.pas（需要手动合并内容）
echo "阶段 2: 创建 fafafa.ssl.base.pas"
echo "  注意: 需要手动合并以下文件的内容:"
echo "    - fafafa.ssl.abstract.types.pas"
echo "    - fafafa.ssl.abstract.intf.pas"
echo
echo "  合并后的 base.pas 应包含:"
echo "    1. 所有类型定义（枚举、记录、集合）"
echo "    2. 所有接口定义（ISSLLibrary, ISSLContext 等）"
echo "    3. 辅助函数"
echo
read -p "按 Enter 继续，或 Ctrl+C 取消..."

# 阶段 3: 列出需要删除的文件
echo
echo "阶段 3: 准备删除的文件"
echo "  以下文件将被删除（已合并到 base.pas）:"
echo "    - fafafa.ssl.abstract.types.pas"
echo "    - fafafa.ssl.abstract.intf.pas"
echo "    - fafafa.ssl.types.pas (转发层)"
echo "    - fafafa.ssl.intf.pas (转发层)"
echo
read -p "确认删除这些文件? (y/N) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -f fafafa.ssl.abstract.types.pas
    rm -f fafafa.ssl.abstract.intf.pas
    rm -f fafafa.ssl.types.pas
    rm -f fafafa.ssl.intf.pas
    echo "✓ 文件已删除"
else
    echo "✗ 跳过删除"
fi
echo

# 阶段 4: 提示需要手动替换
echo "阶段 4: 需要手动替换所有 uses 子句"
echo
echo "在所有 .pas 文件中查找并替换:"
echo "  fafafa.ssl.abstract.types  → fafafa.ssl.base"
echo "  fafafa.ssl.abstract.intf   → fafafa.ssl.base"
echo "  fafafa.ssl.types           → fafafa.ssl.base"
echo "  fafafa.ssl.intf            → fafafa.ssl.base"
echo
echo "可以使用以下命令自动替换:"
echo "  sed -i 's/fafafa\.ssl\.abstract\.types/fafafa.ssl.base/g' *.pas"
echo "  sed -i 's/fafafa\.ssl\.abstract\.intf/fafafa.ssl.base/g' *.pas"
echo

echo "=== 重构完成 ==="
echo
echo "最终结构:"
echo "├── fafafa.ssl.base.pas      (类型 + 接口)"
echo "├── fafafa.ssl.factory.pas   (工厂函数)"
echo "├── fafafa.ssl.pas           (主入口)"
echo "└── ..."
