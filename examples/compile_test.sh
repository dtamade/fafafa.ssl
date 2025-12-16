#!/bin/bash
# fafafa.ssl 示例编译测试脚本
# 
# 用途: 快速编译和测试所有示例程序
# 使用方法: ./compile_test.sh

set -e  # 遇到错误立即退出

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo "=========================================="
echo "fafafa.ssl 示例编译测试"
echo "=========================================="
echo

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 统计变量
TOTAL=0
PASSED=0
FAILED=0

# 编译函数
compile_example() {
    local FILE=$1
    local NAME=$(basename "$FILE" .pas)
    
    TOTAL=$((TOTAL + 1))
    echo -n "编译 $NAME... "
    
    if fpc $FPC_FLAGS "$FILE" > /tmp/compile_${NAME}.log 2>&1; then
        echo -e "${GREEN}✓ 成功${NC}"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ 失败${NC}"
        echo "  错误信息:"
        tail -5 /tmp/compile_${NAME}.log | sed 's/^/  /'
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# 检查FPC是否安装
if ! command -v fpc &> /dev/null; then
    echo -e "${RED}错误: 未找到 fpc 编译器${NC}"
    echo "请安装 Free Pascal:"
    echo "  Ubuntu/Debian: sudo apt-get install fpc"
    echo "  CentOS/RHEL:   sudo yum install fpc"
    echo "  macOS:         brew install fpc"
    exit 1
fi

echo "Free Pascal 版本:"
fpc -iV
echo

# 检查源代码目录
if [ ! -d "${REPO_ROOT}/src" ]; then
    echo -e "${RED}错误: 未找到 src 目录${NC}"
    exit 1
fi

FPC_FLAGS="-Fu${REPO_ROOT}/src -Fu${REPO_ROOT}/src/openssl -Mobjfpc"

# 切换到示例目录
cd "${SCRIPT_DIR}"

echo "=========================================="
echo "1. 编译生产级示例"
echo "=========================================="
echo

cd production
if [ -f "https_client_simple.pas" ]; then
    compile_example "https_client_simple.pas"
fi

if [ -f "https_client_post.pas" ]; then
    compile_example "https_client_post.pas"
fi

if [ -f "https_client_auth.pas" ]; then
    compile_example "https_client_auth.pas"
fi

if [ -f "https_client_session.pas" ]; then
    compile_example "https_client_session.pas"
fi

if [ -f "https_server_simple.pas" ]; then
    compile_example "https_server_simple.pas"
fi

cd ..

echo
echo "=========================================="
echo "2. 编译HTTPS客户端示例"
echo "=========================================="
echo

if [ -d "https_client" ]; then
    cd https_client
    for file in https_client_simple.pas \
                https_client_post.pas \
                https_client_auth.pas \
                https_client_session.pas; do
        if [ -f "$file" ]; then
            compile_example "$file"
        fi
    done
    cd ..
else
    echo "找不到 https_client 目录，跳过"
fi

echo
echo "=========================================="
echo "3. 编译HTTPS服务器示例"
echo "=========================================="
echo

if [ -d "https_server" ]; then
    cd https_server
    for file in https_server_simple.pas \
                https_server_mtls.pas \
                https_server_alpn.pas; do
        if [ -f "$file" ]; then
            compile_example "$file"
        fi
    done
    cd ..
else
    echo "找不到 https_server 目录，跳过"
fi

echo
echo "=========================================="
echo "4. 编译验证示例"
echo "=========================================="
echo

cd validation
if [ -f "real_world_test.pas" ]; then
    compile_example "real_world_test.pas"
fi
cd ..

echo
echo "=========================================="
echo "5. 编译演示示例"
echo "=========================================="
echo

if [ -f "simple_https_demo.pas" ]; then
    compile_example "simple_https_demo.pas"
fi

echo
echo "=========================================="
echo "编译测试结果"
echo "=========================================="
echo "总计: $TOTAL"
echo -e "${GREEN}成功: $PASSED${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}失败: $FAILED${NC}"
else
    echo -e "${GREEN}失败: 0${NC}"
fi

if [ $FAILED -eq 0 ]; then
    echo
    echo -e "${GREEN}✓ 所有示例编译成功！${NC}"
    echo
    echo "下一步:"
    echo "  1. 运行示例: cd production && ./https_client_simple https://www.google.com"
    echo "  2. 运行真实网站测试: cd validation && ./real_world_test"
    exit 0
else
    echo
    echo -e "${YELLOW}⚠ 部分示例编译失败，请检查错误信息${NC}"
    exit 1
fi



