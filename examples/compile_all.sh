#!/bin/bash

#############################################################################
# 示例程序编译脚本
#
# 功能：
# - 自动编译所有示例程序
# - 支持按类别编译
# - 生成编译报告
# - 支持跨平台编译
#
# 用法：
#   ./examples/compile_all.sh [options]
#
# 选项：
#   --verbose       显示详细输出
#   --category      指定要编译的类别（逗号分隔）
#   --clean         编译前清理 bin 目录
#   --help          显示帮助信息
#############################################################################

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 配置
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXAMPLES_DIR="$PROJECT_ROOT/examples"
BIN_DIR="$EXAMPLES_DIR/bin"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
REPORT_FILE="$EXAMPLES_DIR/compile_report_$TIMESTAMP.txt"

# 选项
VERBOSE=false
CLEAN=false
SPECIFIC_CATEGORIES=""

# 统计
TOTAL_EXAMPLES=0
COMPILED_EXAMPLES=0
FAILED_EXAMPLES=0
SKIPPED_EXAMPLES=0

# 日志函数
log_info() {
  echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$REPORT_FILE"
}

log_success() {
  echo -e "${GREEN}[PASS]${NC} $1" | tee -a "$REPORT_FILE"
}

log_error() {
  echo -e "${RED}[FAIL]${NC} $1" | tee -a "$REPORT_FILE"
}

log_warning() {
  echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$REPORT_FILE"
}

# 显示帮助信息
show_help() {
  cat << EOF
示例程序编译脚本

用法: $0 [选项]

选项:
  --verbose           显示详细编译输出
  --category <类别>   指定要编译的类别（逗号分隔）
                      可用类别: basic, tls, https, crypto, cert, pkcs, winssl, production
  --clean             编译前清理 bin 目录
  --help              显示此帮助信息

示例:
  $0                                    # 编译所有示例
  $0 --category basic,tls               # 只编译基础和 TLS 示例
  $0 --clean --verbose                  # 清理后编译，显示详细输出
  $0 --category https --verbose         # 编译 HTTPS 示例，显示详细输出

类别说明:
  basic       - 基础示例（hello_ssl, simple_test 等）
  tls         - TLS 连接示例（01_tls_client, example_tls_client 等）
  https       - HTTPS 客户端/服务器示例
  crypto      - 加密工具示例（hash, encryption, signature 等）
  cert        - 证书管理示例
  pkcs        - PKCS 示例（PKCS#7, PKCS#12）
  winssl      - WinSSL 示例（仅 Windows）
  production  - 生产级示例

EOF
  exit 0
}

# 解析命令行参数
while [[ $# -gt 0 ]]; do
  case $1 in
    --verbose)
      VERBOSE=true
      shift
      ;;
    --clean)
      CLEAN=true
      shift
      ;;
    --category)
      SPECIFIC_CATEGORIES="$2"
      shift 2
      ;;
    --help)
      show_help
      ;;
    *)
      echo "未知选项: $1"
      echo "使用 --help 查看帮助信息"
      exit 1
      ;;
  esac
done

# 创建 bin 目录
mkdir -p "$BIN_DIR"

# 清理 bin 目录
if [ "$CLEAN" = true ]; then
  log_info "清理 bin 目录..."
  rm -f "$BIN_DIR"/*.o "$BIN_DIR"/*.ppu "$BIN_DIR"/*.compiled
  find "$BIN_DIR" -type f -executable -delete 2>/dev/null || true
fi

# 编译示例程序
compile_example() {
  local example_file=$1
  local example_name=$(basename "$example_file" .pas)
  local output_file="$BIN_DIR/$example_name"
  local compile_log="$BIN_DIR/${example_name}_compile.log"

  if [ "$VERBOSE" = true ]; then
    log_info "编译 $example_name..."
  fi

  # 平台特定的编译参数
  local platform_flags=""

  # 检测操作系统
  if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS: 添加 OpenSSL 3.x 库路径
    if command -v brew &> /dev/null; then
      local openssl_prefix=$(brew --prefix openssl@3 2>/dev/null)
      if [ -n "$openssl_prefix" ]; then
        platform_flags="-Fl$openssl_prefix/lib -Fi$openssl_prefix/include"
        platform_flags="$platform_flags -k-rpath -k$openssl_prefix/lib"
        export DYLD_LIBRARY_PATH="$openssl_prefix/lib:$DYLD_LIBRARY_PATH"
      fi
    fi
  elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "win32" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
    # Windows: 检查 OpenSSL 安装位置
    local openssl_paths=(
      "C:/Program Files/OpenSSL-Win64"
      "C:/OpenSSL-Win64"
      "C:/Program Files/OpenSSL"
      "C:/OpenSSL"
    )

    for openssl_path in "${openssl_paths[@]}"; do
      if [ -d "$openssl_path/lib" ]; then
        platform_flags="-Fl$openssl_path/lib -Fi$openssl_path/include"
        export PATH="$openssl_path/bin:$PATH"
        break
      fi
    done
  fi

  # 编译
  if fpc -Mobjfpc -Sh -O2 \
    -Fu"$PROJECT_ROOT/src" \
    -Fu"$PROJECT_ROOT/src/openssl" \
    -Fu"$PROJECT_ROOT/src/winssl" \
    -Fu"$EXAMPLES_DIR" \
    -Fi"$PROJECT_ROOT/src" \
    -FE"$BIN_DIR" \
    $platform_flags \
    "$example_file" > "$compile_log" 2>&1; then

    if [ "$VERBOSE" = true ]; then
      log_success "$example_name: 编译成功"
    fi
    return 0
  else
    log_error "$example_name: 编译失败"
    if [ "$VERBOSE" = true ] && [ -f "$compile_log" ]; then
      echo "编译错误详情:" >> "$REPORT_FILE"
      tail -20 "$compile_log" >> "$REPORT_FILE"
    fi
    return 1
  fi
}

# 获取类别的示例文件
get_category_examples() {
  local category=$1
  case "$category" in
    basic)
      echo "hello_ssl.pas simple_test.pas ultra_simple_test.pas simple_ssl_connection.pas"
      ;;
    tls)
      echo "01_tls_client.pas example_tls_client.pas simple_https_demo.pas session_reuse_example.pas"
      ;;
    https)
      find "$EXAMPLES_DIR/https_client" -name "*.pas" -type f
      find "$EXAMPLES_DIR/https_server" -name "*.pas" -type f
      echo "$EXAMPLES_DIR/04_https_rest_client.pas"
      echo "$EXAMPLES_DIR/05_https_server.pas"
      echo "$EXAMPLES_DIR/https_simple_get.pas"
      echo "$EXAMPLES_DIR/https_client_production.pas"
      ;;
    crypto)
      echo "03_file_encryption.pas 06_digital_signature.pas example_crypto_simple.pas"
      echo "example_crypto_working.pas example_aes_gcm_aead.pas hash_calculator.pas"
      echo "file_encrypt_tool.pas password_hash.pas password_hash_v2.pas"
      find "$EXAMPLES_DIR/file_encrypt" -name "*.pas" -type f
      find "$EXAMPLES_DIR/digital_signature" -name "*.pas" -type f
      find "$EXAMPLES_DIR/password_hash" -name "*.pas" -type f
      ;;
    cert)
      echo "02_generate_certificate.pas 07_certificate_chain.pas 10_cert_renewal.pas"
      echo "certificate_verification_example.pas cert_info_viewer.pas pem_der_converter.pas"
      ;;
    pkcs)
      echo "pkcs7_sign_example.pas pkcs7_encrypt_example.pas pkcs7_sign_encrypt_example.pas"
      echo "pkcs12_example.pas"
      ;;
    winssl)
      echo "09_winssl_fips.pas winssl_https_downloader.pas winssl_rest_client.pas"
      echo "winssl_health_checker.pas"
      ;;
    production)
      find "$EXAMPLES_DIR/production" -name "*.pas" -type f
      ;;
    *)
      echo ""
      ;;
  esac
}

# 开始编译
echo "========================================" | tee "$REPORT_FILE"
echo "示例程序编译报告" | tee -a "$REPORT_FILE"
echo "时间: $(date)" | tee -a "$REPORT_FILE"
echo "========================================" | tee -a "$REPORT_FILE"
echo "" | tee -a "$REPORT_FILE"

# 确定要编译的示例
if [ -n "$SPECIFIC_CATEGORIES" ]; then
  IFS=',' read -ra CATEGORIES <<< "$SPECIFIC_CATEGORIES"
  EXAMPLES_TO_COMPILE=()

  for category in "${CATEGORIES[@]}"; do
    log_info "收集类别: $category"
    category_examples=$(get_category_examples "$category")

    if [ -z "$category_examples" ]; then
      log_warning "未知类别: $category"
      continue
    fi

    for example in $category_examples; do
      if [ -f "$example" ]; then
        EXAMPLES_TO_COMPILE+=("$example")
      elif [ -f "$EXAMPLES_DIR/$example" ]; then
        EXAMPLES_TO_COMPILE+=("$EXAMPLES_DIR/$example")
      fi
    done
  done
else
  # 编译所有示例（排除子目录中的辅助文件）
  EXAMPLES_TO_COMPILE=($(find "$EXAMPLES_DIR" -maxdepth 1 -name "*.pas" -type f))

  # 添加子目录中的示例
  for subdir in https_client https_server production file_encrypt digital_signature password_hash hmac_tool validation; do
    if [ -d "$EXAMPLES_DIR/$subdir" ]; then
      EXAMPLES_TO_COMPILE+=($(find "$EXAMPLES_DIR/$subdir" -name "*.pas" -type f))
    fi
  done
fi

# 去重
EXAMPLES_TO_COMPILE=($(printf '%s\n' "${EXAMPLES_TO_COMPILE[@]}" | sort -u))

TOTAL_EXAMPLES=${#EXAMPLES_TO_COMPILE[@]}
log_info "找到 $TOTAL_EXAMPLES 个示例程序"
echo "" | tee -a "$REPORT_FILE"

# 编译每个示例
for example_file in "${EXAMPLES_TO_COMPILE[@]}"; do
  example_name=$(basename "$example_file" .pas)

  # 跳过辅助模块（不是可执行程序）
  if [[ "$example_name" == "fafafa.examples."* ]] || \
     [[ "$example_name" == *"_common" ]]; then
    if [ "$VERBOSE" = true ]; then
      log_warning "$example_name: 跳过（辅助模块）"
    fi
    SKIPPED_EXAMPLES=$((SKIPPED_EXAMPLES + 1))
    continue
  fi

  if compile_example "$example_file"; then
    COMPILED_EXAMPLES=$((COMPILED_EXAMPLES + 1))
  else
    FAILED_EXAMPLES=$((FAILED_EXAMPLES + 1))
  fi
done

# 生成总结
echo "" | tee -a "$REPORT_FILE"
echo "========================================" | tee -a "$REPORT_FILE"
echo "编译总结" | tee -a "$REPORT_FILE"
echo "========================================" | tee -a "$REPORT_FILE"
echo "总示例数: $TOTAL_EXAMPLES" | tee -a "$REPORT_FILE"
echo "编译成功: $COMPILED_EXAMPLES" | tee -a "$REPORT_FILE"
echo "编译失败: $FAILED_EXAMPLES" | tee -a "$REPORT_FILE"
echo "跳过: $SKIPPED_EXAMPLES" | tee -a "$REPORT_FILE"

if [ $TOTAL_EXAMPLES -gt 0 ]; then
  SUCCESS_RATE=$(awk "BEGIN {printf \"%.1f\", $COMPILED_EXAMPLES * 100.0 / $TOTAL_EXAMPLES}")
  echo "成功率: $SUCCESS_RATE%" | tee -a "$REPORT_FILE"
fi

echo "" | tee -a "$REPORT_FILE"
echo "详细报告: $REPORT_FILE" | tee -a "$REPORT_FILE"

# 退出码
if [ $FAILED_EXAMPLES -gt 0 ]; then
  log_error "编译完成，但有 $FAILED_EXAMPLES 个示例失败"
  exit 1
else
  log_success "所有示例编译成功！"
  exit 0
fi
