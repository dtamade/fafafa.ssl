#!/bin/bash
# macOS 构建脚本 - fafafa.ssl
# 用于在 macOS 平台上编译和测试项目

set -e

echo "=========================================="
echo "  fafafa.ssl - macOS Build Script"
echo "=========================================="
echo ""

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 检查 Free Pascal 编译器
echo "检查 Free Pascal 编译器..."
if ! command -v fpc &> /dev/null; then
    echo -e "${RED}错误: 未找到 FPC 编译器${NC}"
    echo "请使用以下命令安装:"
    echo "  brew install fpc"
    echo "或从官网下载: https://www.freepascal.org/download.html"
    exit 1
fi

FPC_VERSION=$(fpc -iV)
echo -e "${GREEN}✓ FPC 版本: $FPC_VERSION${NC}"
echo ""

# 检查 OpenSSL
echo "检查 OpenSSL 库..."
if ! command -v openssl &> /dev/null; then
    echo -e "${RED}错误: 未找到 OpenSSL${NC}"
    echo "请使用以下命令安装:"
    echo "  brew install openssl@3"
    exit 1
fi

OPENSSL_VERSION=$(openssl version)
echo -e "${GREEN}✓ $OPENSSL_VERSION${NC}"

# 检测 OpenSSL 库路径
if [ -d "/opt/homebrew/opt/openssl@3" ]; then
    # Apple Silicon (M1/M2)
    OPENSSL_PATH="/opt/homebrew/opt/openssl@3"
elif [ -d "/usr/local/opt/openssl@3" ]; then
    # Intel Mac
    OPENSSL_PATH="/usr/local/opt/openssl@3"
else
    echo -e "${YELLOW}警告: 未找到标准 OpenSSL 路径${NC}"
    OPENSSL_PATH=""
fi

if [ -n "$OPENSSL_PATH" ]; then
    echo -e "${GREEN}✓ OpenSSL 路径: $OPENSSL_PATH${NC}"
    export DYLD_LIBRARY_PATH="$OPENSSL_PATH/lib:$DYLD_LIBRARY_PATH"
fi
echo ""

# 显示系统信息
echo "系统信息:"
echo "  架构: $(uname -m)"
echo "  系统: $(uname -s) $(uname -r)"
echo ""

# 编译选项
echo "开始编译..."
echo ""

# 创建输出目录
mkdir -p tests/bin
mkdir -p tests/lib

# 编译核心测试 (如果存在 run_core_tests.sh)
if [ -f "tests/run_core_tests.sh" ]; then
    echo "运行核心测试编译脚本..."
    cd tests
    chmod +x run_core_tests.sh
    ./run_core_tests.sh
    cd ..
else
    echo -e "${YELLOW}注意: 未找到 tests/run_core_tests.sh${NC}"
    echo "尝试手动编译测试文件..."

    # 手动编译一些核心测试
    TEST_FILES=(
        "test_aes.pas"
        "test_sha.pas"
        "test_rsa_simple.pas"
        "test_openssl_load.pas"
    )

    COMPILED=0
    FAILED=0

    for test in "${TEST_FILES[@]}"; do
        if [ -f "tests/$test" ]; then
            echo "编译 $test..."
            if fpc -Mobjfpc -Sh -Fu"src" -Fi"src" -FU"tests/lib" -FE"tests/bin" "tests/$test" > /dev/null 2>&1; then
                echo -e "${GREEN}✓ $test 编译成功${NC}"
                ((COMPILED++))
            else
                echo -e "${RED}✗ $test 编译失败${NC}"
                ((FAILED++))
            fi
        fi
    done

    echo ""
    echo "编译结果: $COMPILED 成功, $FAILED 失败"
fi

echo ""
echo "=========================================="
echo -e "${GREEN}构建完成!${NC}"
echo "=========================================="
echo ""
echo "运行测试:"
echo "  cd tests/bin"
echo "  ./test_aes"
echo ""
echo "查看所有测试:"
echo "  ls -la tests/bin/"
echo ""
