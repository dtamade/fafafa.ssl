# fafafa.ssl 示例程序

本目录包含 fafafa.ssl 的示例程序，展示库的各种功能和用法。

## 📋 示例列表

### 基础示例

| # | 示例名称 | 文件 | 描述 | 难度 |
|---|---------|------|------|------|
| 0 | Hello SSL | `hello_ssl.pas` | 快速验证 OpenSSL 环境 | ⭐ |
| 1 | TLS 客户端 | `01_tls_client.pas` | 连接到 HTTPS 服务器 | ⭐ |
| 2 | 证书生成 | `02_generate_certificate.pas` | 生成自签名证书和私钥 | ⭐⭐ |
| 3 | 文件加密 | `03_file_encryption.pas` | AES-256-GCM 文件加密/解密 | ⭐⭐ |

### 实际场景（计划中）

| # | 示例名称 | 文件 | 描述 | 难度 |
|---|---------|------|------|------|
| 4 | HTTPS REST API | `04_https_rest_api.pas` | REST API 客户端 | ⭐⭐ |
| 5 | HTTPS Web 服务器 | `05_https_server.pas` | 简单的 HTTPS 服务器 | ⭐⭐⭐ |
| 6 | 数字签名 | `06_digital_signature.pas` | 文件签名和验证 | ⭐⭐ |
| 7 | 证书验证 | `07_certificate_validation.pas` | 完整的证书链验证 | ⭐⭐⭐ |

### 企业场景（计划中）

| # | 示例名称 | 文件 | 描述 | 难度 |
|---|---------|------|------|------|
| 8 | 双向 TLS | `08_mutual_tls.pas` | 客户端证书认证 | ⭐⭐⭐ |
| 9 | WinSSL FIPS | `09_winssl_fips.pas` | Windows FIPS 模式 | ⭐⭐⭐ |
| 10 | 证书轮换 | `10_cert_rotation.pas` | 自动证书更新 | ⭐⭐⭐⭐ |

## 🚀 快速开始

### 编译示例

```bash
# 编译单个示例
fpc -Fusrc -Fusrc\openssl examples\hello_ssl.pas

# 或使用 Lazarus
lazbuild examples\hello_ssl.lpi
```

### 运行示例

```bash
# 快速验证
examples\hello_ssl.exe

# TLS 客户端
examples\01_tls_client.exe

# 生成证书
examples\02_generate_certificate.exe

# 文件加密
examples\03_file_encryption.exe encrypt input.txt output.enc password123
examples\03_file_encryption.exe decrypt output.enc recovered.txt password123
```

## 📚 示例详解

### 示例 0: Hello SSL

**用途**: 验证 OpenSSL 环境配置是否正确

**学习内容**:
- SSL 库加载
- 版本信息获取
- 基本错误处理

**运行时间**: < 1 秒

**代码片段**:
```pascal
if LoadOpenSSLLibrary then
  WriteLn('Version: ', GetOpenSSLVersion);
```

---

### 示例 1: TLS 客户端连接

**用途**: 连接到 HTTPS 服务器并发送请求

**学习内容**:
- 创建 SSL 上下文
- 配置 TLS 参数
- 执行 TLS 握手
- 证书验证
- 加密数据传输

**运行时间**: 2-5 秒（取决于网络）

**代码片段**:
```pascal
LContext := LLib.CreateContext(sslCtxClient);
LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
LConn := LContext.CreateConnection(socket);
if LConn.Connect then
  LConn.WriteString('GET / HTTP/1.1'#13#10#13#10);
```

**关键点**:
- 自动加载系统 CA 证书
- 验证服务器证书
- 检查主机名匹配

---

### 示例 2: 证书生成与自签名

**用途**: 生成 RSA 密钥对和自签名证书

**学习内容**:
- RSA 密钥生成
- X.509 证书创建
- 证书字段设置
- 证书签名
- PEM 格式保存

**运行时间**: 5-10 秒（取决于密钥大小）

**代码片段**:
```pascal
LPrivKey := EVP_PKEY_new();
RSA_generate_key_ex(LRsa, 2048, LBn, nil);
LCert := X509_new();
X509_sign(LCert, LPrivKey, EVP_sha256());
```

**输出**:
- `server.key` - 私钥文件（2048 位 RSA）
- `server.crt` - 自签名证书（365 天有效期）

**安全提示**:
- 私钥文件权限应设置为 400
- 不要提交私钥到版本控制
- 自签名证书仅用于测试

---

### 示例 3: 文件加密与解密

**用途**: 使用密码加密和解密文件

**学习内容**:
- AES-256-GCM 加密
- PBKDF2 密钥派生
- 认证加密（AEAD）
- 文件流处理
- 自定义文件格式

**运行时间**: 取决于文件大小（~1 MB/s）

**代码片段**:
```pascal
// 加密
LKey := DeriveKey(aPassword, LSalt);
EVP_EncryptInit_ex(LCtx, EVP_aes_256_gcm(), nil, @LKey[0], @LIV[0]);
EVP_EncryptUpdate(LCtx, @LCipherChunk[0], @LOutLen, @LPlainChunk[0], LBytesRead);

// 解密
EVP_DecryptInit_ex(LCtx, EVP_aes_256_gcm(), nil, @LKey[0], @LIV[0]);
EVP_DecryptUpdate(LCtx, @LPlainChunk[0], @LOutLen, @LCipherChunk[0], LBytesRead);
```

**文件格式**:
```
Header (64 bytes):
  - Magic: 'FAFAFA01' (8 bytes)
  - Version: 1 (1 byte)
  - Algorithm: 1 (AES-256-GCM) (1 byte)
  - Reserved: (2 bytes)
  - Salt: (16 bytes)
  - IV: (12 bytes)
  - Tag: (16 bytes)
Data:
  - Encrypted content (variable)
```

**安全性**:
- 认证加密防止篡改
- 随机 Salt 防止字典攻击
- PBKDF2 增加破解难度
- 使用强密码（12+ 字符）

---

## 🔧 编译所有示例

创建批处理脚本 `build_examples.bat`:

```batch
@echo off
setlocal

set SRC_PATH=src
set OPENSSL_PATH=src\openssl

echo Building all examples...
echo.

for %%f in (examples\*.pas) do (
    echo Building %%f...
    fpc -Fu%SRC_PATH% -Fu%OPENSSL_PATH% -FEexamples "%%f"
    if errorlevel 1 (
        echo [ERROR] Failed to build %%f
        exit /b 1
    )
)

echo.
echo All examples built successfully!
```

或在 Linux/macOS:

```bash
#!/bin/bash

SRC_PATH="src"
OPENSSL_PATH="src/openssl"

echo "Building all examples..."
echo

for file in examples/*.pas; do
    echo "Building $file..."
    fpc -Fu$SRC_PATH -Fu$OPENSSL_PATH -FEexamples "$file"
    if [ $? -ne 0 ]; then
        echo "[ERROR] Failed to build $file"
        exit 1
    fi
done

echo
echo "All examples built successfully!"
```

## 📖 相关文档

- [QUICK_START.md](../QUICK_START.md) - 快速入门指南
- [API_REFERENCE.md](../docs/API_REFERENCE.md) - 完整 API 参考
- [USER_GUIDE.md](../docs/USER_GUIDE.md) - 用户指南
- [SECURITY_GUIDE.md](../docs/SECURITY_GUIDE.md) - 安全最佳实践

## 🐛 故障排除

### OpenSSL 找不到

**问题**: `Failed to load OpenSSL library`

**解决方案**:
1. 确保 OpenSSL 已安装
2. Windows: 将 DLL 放到 PATH 或程序目录
3. Linux: `sudo apt install libssl3`

详见 [TROUBLESHOOTING.md](../docs/TROUBLESHOOTING.md)

### 编译错误

**问题**: `Error: Identifier not found`

**解决方案**:
1. 确保使用 `-Fusrc` 和 `-Fusrc\openssl` 参数
2. 检查 FPC 版本（需要 3.2.0+）
3. 使用 `{$mode objfpc}{$H+}` 编译指令

### 连接失败

**问题**: TLS 握手失败

**解决方案**:
1. 检查网络连接
2. 确认服务器地址和端口
3. 检查防火墙设置
4. 查看详细错误信息

## 💡 贡献示例

欢迎贡献新的示例！

**要求**:
1. 遵循 [WARP.md](../WARP.md) 代码规范
2. 包含详细注释
3. 提供使用说明
4. 测试通过

**提交流程**:
1. Fork 项目
2. 创建示例分支
3. 编写示例代码
4. 提交 Pull Request

## 📊 示例统计

- **已完成**: 4 个
- **计划中**: 7 个
- **总计**: 11 个
- **代码行数**: ~1,500 行
- **覆盖功能**: 70%+

---

**最后更新**: 2025-10-24  
**维护者**: fafafa.ssl 团队
