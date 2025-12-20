#!/bin/bash
# fafafa.ssl 结构重构脚本
# 方案 C: 扁平但分组（最小改动）

set -e

echo "=== fafafa.ssl 结构重构 ==="
echo "方案: 扁平但分组（最小改动）"
echo

cd src

# 阶段 1: 清理
echo "阶段 1: 清理编译产物和临时文件..."
rm -f *.o *.ppu *.bak *_new.pas *_old.pas
echo "✓ 清理完成"
echo

# 阶段 2: 创建目录结构
echo "阶段 2: 创建目录结构..."
mkdir -p abstract
mkdir -p openssl/api
mkdir -p winssl
mkdir -p common
echo "✓ 目录创建完成"
echo

# 阶段 3: 移动文件（保守方案）
echo "阶段 3: 移动文件..."

# 移动 abstract 文件
echo "  移动 abstract 文件..."
mv -v fafafa.ssl.abstract.*.pas abstract/ 2>/dev/null || true

# 移动 OpenSSL API 文件
echo "  移动 OpenSSL API 文件..."
mv -v fafafa.ssl.openssl.api.*.pas openssl/api/ 2>/dev/null || true

# 移动其他 OpenSSL 文件到 openssl/
echo "  移动其他 OpenSSL 文件..."
mv -v fafafa.ssl.openssl.*.pas openssl/ 2>/dev/null || true

# 移动 WinSSL 文件
echo "  移动 WinSSL 文件..."
mv -v fafafa.ssl.winssl.*.pas winssl/ 2>/dev/null || true

# 移动通用工具文件
echo "  移动通用工具文件..."
for file in log.pas utils.pas ringbuffer.pas certchain.pas; do
    if [ -f "fafafa.ssl.$file" ]; then
        mv -v "fafafa.ssl.$file" common/
    fi
done

echo "✓ 文件移动完成"
echo

# 阶段 4: 创建 README
echo "阶段 4: 创建目录说明..."
cat > abstract/README.md << 'README'
# Abstract Layer

Platform-independent interface definitions and types.

## Files
- `types.pas` - Base types and enums
- `interfaces.pas` - ISSLLibrary, ISSLContext, ISSLConnection, etc.
README

cat > openssl/README.md << 'README'
# OpenSSL Backend

OpenSSL library bindings and implementation.

## Structure
- `api/` - OpenSSL C API function bindings
- Other files - Implementation of abstract interfaces
README

cat > openssl/api/README.md << 'README'
# OpenSSL API Bindings

Direct bindings to OpenSSL C library functions.

Each file corresponds to an OpenSSL module:
- `core.pas` - SSL/TLS core functions
- `x509.pas` - X509 certificate functions
- `bio.pas` - BIO I/O functions
- etc.
README

cat > winssl/README.md << 'README'
# WinSSL Backend

Windows Schannel (WinSSL) implementation.

Platform: Windows only
README

cat > common/README.md << 'README'
# Common Utilities

Shared utilities used by all backends.

- `log.pas` - Logging system
- `utils.pas` - Helper functions
- `ringbuffer.pas` - Ring buffer implementation
- `certchain.pas` - Certificate chain validation
README

echo "✓ README 创建完成"
echo

# 总结
echo "=== 重构完成 ==="
echo
echo "新结构:"
tree -L 2 -d .
echo
echo "下一步:"
echo "1. 更新所有 uses 子句中的路径"
echo "2. 更新 .lpi/.lpk 中的 SearchPaths"
echo "3. 重新编译测试"
echo
echo "注意: uses 子句需要手动更新！"
