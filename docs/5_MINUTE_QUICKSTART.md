# fafafa.ssl 5 分钟快速开始

**目标**: 在 5 分钟内完成 fafafa.ssl 的安装、验证和第一个示例运行。

## 前置要求

- FreePascal 3.2.0+
- Linux/macOS: OpenSSL 1.1.1+ 或 3.x
- Windows: 可选（WinSSL 零依赖）

## 第 1 步：验证环境（1 分钟）

```bash
# 检查 FreePascal
fpc -iV
# 应显示: 3.2.0 或更高版本

# 检查 OpenSSL（Linux/macOS）
openssl version
# 应显示: OpenSSL 1.1.1 或 3.x

# Windows 用户可跳过 OpenSSL 检查（使用 WinSSL）
```

## 第 2 步：克隆项目（1 分钟）

```bash
git clone https://github.com/your-org/fafafa.ssl.git
cd fafafa.ssl
```

## 第 3 步：运行第一个示例（2 分钟）

### 示例 1: 验证 OpenSSL 环境

```bash
# 编译
fpc -Mobjfpc -Sh -Fu./src -Fu./src/openssl examples/hello_ssl.pas

# 运行
./hello_ssl
```

**预期输出**:
```
=============================================================================
  fafafa.ssl - OpenSSL Pascal Bindings
  Quick Start Example
=============================================================================

[Step 1] Loading OpenSSL library...
         SUCCESS

[Step 2] Get version info...
         Version: OpenSSL 3.0.2 15 Mar 2022

[Step 3] Check backend support...
         - OpenSSL:  Available

=============================================================================
  Test Result: PASSED
  Your environment is correctly configured!
=============================================================================
```

### 示例 2: 计算 SHA-256 哈希

```bash
# 编译
fpc -Mobjfpc -Sh -Fu./src examples/hash_calculator.pas

# 运行
./hash_calculator "Hello, fafafa.ssl!"
```

**预期输出**:
```
SHA-256: a1b2c3d4e5f6...
```

### 示例 3: HTTPS 客户端（需要网络）

```bash
# 编译
fpc -Mobjfpc -Sh -Fu./src -Fu./src/openssl -Fu./examples examples/01_tls_client.pas

# 运行
./01_tls_client https://www.example.com/
```

**预期输出**:
```
================================================================================
示例 1: TLS 客户端连接
URL: https://www.example.com/
================================================================================

Backend: OpenSSL / OpenSSL 3.0.2 15 Mar 2022
连接 TCP: www.example.com:443 ...
执行 TLS 握手 (SNI=www.example.com) ...
TLS 版本: TLS 1.3
密码套件: TLS_AES_256_GCM_SHA384
证书验证: ok

服务器证书:
  主题: CN=www.example.com
  颁发者: CN=DigiCert TLS RSA SHA256 2020 CA1
  有效期至: 2026-03-15 23:59:59

发送 HTTP/1.1 请求: GET /
收到响应: 1256 bytes
响应体大小(粗略): 648 bytes

响应头预览(前 10 行)：
--------------------------------------------------------------------------------
  HTTP/1.1 200 OK
  Content-Type: text/html; charset=UTF-8
  Content-Length: 648
  ...
--------------------------------------------------------------------------------
```

## 第 4 步：探索更多示例（1 分钟）

```bash
# 查看所有示例
ls examples/*.pas

# 查看示例索引
cat examples/EXAMPLES_INDEX.md

# 批量编译示例
chmod +x examples/compile_all.sh
./examples/compile_all.sh --category basic
```

## 常见问题

### Q: 编译失败，提示找不到 OpenSSL

**Linux (Ubuntu/Debian)**:
```bash
sudo apt-get install libssl-dev
```

**Linux (Fedora/RHEL)**:
```bash
sudo dnf install openssl-devel
```

**macOS**:
```bash
brew install openssl@3

# 编译时指定 OpenSSL 路径
fpc -Mobjfpc -Sh -Fu./src -Fu./src/openssl \
    -Fl$(brew --prefix openssl@3)/lib \
    -Fi$(brew --prefix openssl@3)/include \
    examples/hello_ssl.pas
```

**Windows**:
- 下载 OpenSSL: https://slproweb.com/products/Win32OpenSSL.html
- 或使用 WinSSL（零依赖）:
```powershell
fpc -Mobjfpc -Sh -Fu.\src -Fu.\src\winssl examples\09_winssl_fips.pas
```

### Q: 运行时提示找不到 libssl.so

**Linux**:
```bash
# 检查 OpenSSL 安装
ldconfig -p | grep libssl

# 如果未找到，安装 OpenSSL
sudo apt-get install libssl3  # Ubuntu/Debian
sudo dnf install openssl-libs  # Fedora/RHEL
```

**macOS**:
```bash
# 设置库路径
export DYLD_LIBRARY_PATH=$(brew --prefix openssl@3)/lib:$DYLD_LIBRARY_PATH
```

### Q: HTTPS 示例连接失败

1. **检查网络连接**:
```bash
ping www.example.com
```

2. **检查证书验证**:
```bash
# 测试 OpenSSL 连接
openssl s_client -connect www.example.com:443
```

3. **使用其他测试站点**:
```bash
./01_tls_client https://httpbin.org/get
```

## 下一步

### 学习路径

1. **初学者**:
   - 阅读 [GETTING_STARTED.md](GETTING_STARTED.md)
   - 运行 [examples/README.md](../examples/README.md) 中的基础示例
   - 学习 [QUICKSTART.md](QUICKSTART.md) 中的核心概念

2. **进阶**:
   - 探索 [examples/EXAMPLES_INDEX.md](../examples/EXAMPLES_INDEX.md) 中的分类示例
   - 学习 [API_REFERENCE.md](API_REFERENCE.md) 中的 API 使用
   - 阅读 [USER_GUIDE.md](USER_GUIDE.md) 了解最佳实践

3. **高级**:
   - 研究 [ARCHITECTURE.md](ARCHITECTURE.md) 了解架构设计
   - 查看 [SECURITY_GUIDE.md](SECURITY_GUIDE.md) 学习安全实践
   - 参与 [CONTRIBUTING.md](../CONTRIBUTING.md) 贡献代码

### 常用示例

| 场景 | 示例文件 | 说明 |
|------|---------|------|
| 验证环境 | `hello_ssl.pas` | 检查 OpenSSL 安装 |
| 哈希计算 | `hash_calculator.pas` | SHA-256/512 哈希 |
| 文件加密 | `03_file_encryption.pas` | AES-256-GCM 加密 |
| TLS 客户端 | `01_tls_client.pas` | HTTPS 连接 |
| 证书生成 | `02_generate_certificate.pas` | 自签名证书 |
| 数字签名 | `06_digital_signature.pas` | RSA 签名验证 |
| HTTPS 服务器 | `05_https_server.pas` | 简单 HTTPS 服务器 |
| 双向 TLS | `08_mutual_tls.pas` | mTLS 认证 |

### 获取帮助

- **文档**: 查看 `docs/` 目录
- **示例**: 查看 `examples/` 目录
- **问题**: 提交 GitHub Issue
- **讨论**: GitHub Discussions

## 性能提示

### 编译优化

```bash
# 使用 -O3 优化级别
fpc -Mobjfpc -Sh -O3 -Fu./src your_app.pas

# 生成更小的可执行文件
fpc -Mobjfpc -Sh -O3 -Xs -XX -Fu./src your_app.pas
```

### 运行时优化

1. **启用会话复用**:
```pascal
// 使用 TSSLContextBuilder 创建共享上下文
Ctx := TSSLContextBuilder.Create
  .WithTLS12And13
  .WithVerifyPeer
  .WithSystemRoots
  .BuildClient;

// 复用上下文进行多次连接
for i := 1 to 10 do
begin
  Connector := TSSLConnector.FromContext(Ctx);
  Stream := Connector.ConnectSocket(Socket, 'example.com');
  // ... 使用连接 ...
  Stream.Free;
end;
```

2. **使用连接池**:
```pascal
// 参考 examples/production/https_client_session.pas
```

## 故障排查

### 编译错误

| 错误 | 原因 | 解决方案 |
|------|------|---------|
| `Fatal: Can't find unit fafafa.ssl` | 未指定源码路径 | 添加 `-Fu./src` |
| `Fatal: Can't find unit OpenSSL` | 未指定 OpenSSL 单元路径 | 添加 `-Fu./src/openssl` |
| `Error: Identifier not found "SSL_CTX_new"` | OpenSSL 库未加载 | 检查 OpenSSL 安装 |

### 运行时错误

| 错误 | 原因 | 解决方案 |
|------|------|---------|
| `Cannot load OpenSSL library` | OpenSSL 未安装或路径错误 | 安装 OpenSSL 或设置 LD_LIBRARY_PATH |
| `Certificate verify failed` | 证书验证失败 | 检查系统根证书或禁用验证（仅测试） |
| `Connection refused` | 目标服务器不可达 | 检查网络连接和防火墙 |

## 总结

恭喜！你已经完成了 fafafa.ssl 的快速开始。

**你已经学会了**:
- ✅ 验证 OpenSSL 环境
- ✅ 编译和运行示例程序
- ✅ 计算哈希值
- ✅ 建立 HTTPS 连接
- ✅ 解决常见问题

**下一步建议**:
1. 浏览 [examples/EXAMPLES_INDEX.md](../examples/EXAMPLES_INDEX.md) 查看所有示例
2. 阅读 [GETTING_STARTED.md](GETTING_STARTED.md) 深入学习
3. 查看 [API_REFERENCE.md](API_REFERENCE.md) 了解完整 API
4. 尝试编写自己的第一个 SSL/TLS 应用

**需要帮助？**
- 查看 [FAQ.md](FAQ.md) 常见问题
- 阅读 [TROUBLESHOOTING.md](TROUBLESHOOTING.md) 故障排查
- 提交 GitHub Issue 获取支持

---

**最后更新**: 2026-01-20
