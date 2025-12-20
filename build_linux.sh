#!/bin/bash
#
# fafafa.ssl Linux构建脚本
# 编译Lazarus包和所有核心模块
#

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 项目根目录
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

echo -e "${BLUE}============================================================${NC}"
echo -e "${BLUE}fafafa.ssl Linux构建脚本${NC}"
echo -e "${BLUE}============================================================${NC}"
echo ""

# 检查FPC安装
echo -n "检查Free Pascal编译器... "
if ! command -v fpc &> /dev/null; then
    echo -e "${RED}✗ 未找到${NC}"
    echo ""
    echo "请安装Free Pascal:"
    echo "  Ubuntu/Debian: sudo apt-get install fpc"
    echo "  Fedora: sudo dnf install fpc"
    echo "  Arch: sudo pacman -S fpc"
    exit 1
fi

FPC_VERSION=$(fpc -iV)
echo -e "${GREEN}✓ ${FPC_VERSION}${NC}"

# 检查OpenSSL
echo -n "检查OpenSSL库... "
if ! ldconfig -p | grep -q libcrypto.so; then
    echo -e "${YELLOW}⚠ 未找到libcrypto.so${NC}"
    echo "  建议安装: sudo apt-get install libssl3 libssl-dev"
else
    echo -e "${GREEN}✓ 已安装${NC}"
fi

# 检查FCL单元路径
echo -n "检查FCL单元路径... "
FPC_BASE="$HOME/freePascal/fpc/units/x86_64-linux"

if [ ! -d "$FPC_BASE" ]; then
    echo -e "${YELLOW}⚠ 未找到自定义路径${NC}"
    echo "  将使用系统默认FCL路径"
    FPC_BASE="/usr/lib/fpc/${FPC_VERSION}"
fi

if [ -d "$FPC_BASE/fcl-base" ] && [ -d "$FPC_BASE/fcl-json" ]; then
    echo -e "${GREEN}✓ FCL可用${NC}"
else
    echo -e "${RED}✗ FCL单元缺失${NC}"
    echo "  请安装: sudo apt-get install fp-units-fcl"
    exit 1
fi

echo ""
echo -e "${BLUE}开始编译...${NC}"
echo ""

# 单元路径配置
UNIT_PATHS="-Fu${FPC_BASE}/rtl-objpas -Fu${FPC_BASE}/fcl-base -Fu${FPC_BASE}/fcl-json -Fusrc"

# 编译选项
FPC_OPTIONS="-Mobjfpc -Scgi -O2 -g -gl -vewnhi"

# 编译测试
echo "运行批量编译验证..."
if python3 scripts/compile_all_modules.py --rebuild; then
    echo -e "${GREEN}✓ 所有核心模块编译成功${NC}"
else
    echo -e "${RED}✗ 部分模块编译失败${NC}"
    echo "  请查看上方错误信息"
    exit 1
fi

echo ""
echo -e "${GREEN}============================================================${NC}"
echo -e "${GREEN}构建成功！${NC}"
echo -e "${GREEN}============================================================${NC}"
echo ""
echo "后续步骤:"
echo "  1. 运行测试: ./run_tests_linux.sh"
echo "  2. 查看示例: examples/"
echo "  3. 阅读文档: docs/"
echo ""

