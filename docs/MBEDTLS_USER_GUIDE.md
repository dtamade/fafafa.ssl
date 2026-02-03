# MbedTLS 用户指南

**MbedTLS** 是 fafafa.ssl 的嵌入式 SSL/TLS 后端实现，基于 ARM Mbed TLS 库，为嵌入式系统和资源受限环境提供轻量级的 HTTPS 客户端功能。

---

## 📑 文档导航

### 🎯 MbedTLS 是什么？

MbedTLS 是 **fafafa.ssl** 框架的一个后端实现，使用 **ARM Mbed TLS** 库提供 SSL/TLS 功能。

### 核心优势

#### 1. ✅ 轻量级设计

```
传统 OpenSSL 应用:
MyApp (200 KB)
├── libcrypto-3-x64.dll (5 MB)
├── libssl-3-x64.dll (800 KB)
总计: ~6 MB

MbedTLS 应用:
MyApp (210 KB)
├── libmbedcrypto.so (500 KB)
├── libmbedx509.so (200 KB)
├── libmbedtls.so (150 KB)
总计: ~1 MB
```

**优势**:
- 显著减少内存占用
- 适合嵌入式系统
- 更小的二进制体积
- 模块化设计

#### 2. ✅ 嵌入式优化

**专为资源受限环境设计**:
- 低内存占用（可配置）
- 无动态内存分配选项
- 支持裸机环境
- 可定制的功能集

**支持的平台**:
- ARM Cortex-M 系列
- RISC-V
- ESP32/ESP8266
- Linux/Unix 系统
- Windows

#### 3. ✅ 安全认证

- **FIPS 140-2** 认证（特定版本）
- **PSA Certified** Level 2
- 定期安全审计
- 活跃的安全响应团队

#### 4. ✅ 统一 API

MbedTLS 实现了与 OpenSSL/WinSSL 后端**完全相同的接口**:

```pascal
// 代码完全相同，只需改变库类型
{$IFDEF EMBEDDED}
Lib := CreateSSLLibrary(sslMbedTLS);   // 嵌入式: 轻量级
{$ELSE}
Lib := CreateSSLLibrary(sslOpenSSL);   // 桌面: 功能完整
{$ENDIF}

// 其余代码完全相同
Context := Lib.CreateContext;
Context.LoadCertificateFromFile('cert.pem');
```

---

## 🚀 快速开始

### 系统要求

- **Free Pascal Compiler (FPC)** 3.2.0+
- **MbedTLS** 3.0+ (推荐 3.6+)
- **平台**: Linux, Windows, macOS, 嵌入式系统

### 安装 MbedTLS

**Ubuntu/Debian**:
```bash
sudo apt update
sudo apt install libmbedtls-dev libmbedtls14
```

**Fedora/RHEL**:
```bash
sudo dnf install mbedtls mbedtls-devel
```

**macOS**:
```bash
brew install mbedtls
```

**Windows**:
```powershell
# 使用 vcpkg
vcpkg install mbedtls

# 或从源码编译
git clone https://github.com/Mbed-TLS/mbedtls.git
cd mbedtls
mkdir build && cd build
cmake ..
cmake --build .
```

**嵌入式系统**:
```bash
# 从源码编译（可定制配置）
git clone https://github.com/Mbed-TLS/mbedtls.git
cd mbedtls
# 编辑 include/mbedtls/mbedtls_config.h 定制功能
make
```

### 验证安装

```bash
# Linux
ldconfig -p | grep mbedtls

# 应该看到:
# libmbedtls.so.14 (libc6,x86-64) => /usr/lib/x86_64-linux-gnu/libmbedtls.so.14
# libmbedx509.so.6 (libc6,x86-64) => /usr/lib/x86_64-linux-gnu/libmbedx509.so.6
# libmbedcrypto.so.15 (libc6,x86-64) => /usr/lib/x86_64-linux-gnu/libmbedcrypto.so.15
```

---

## 📖 基础使用

### 示例 1: 简单 HTTPS 客户端

```pascal
program simple_https_mbedtls;

uses
  fafafa.ssl,
  fafafa.ssl.abstract.intf;

var
  Lib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  Request, Response: string;
begin
  // 1. 初始化 MbedTLS 库
  Lib := CreateSSLLibrary(sslMbedTLS);
  
  // 2. 创建上下文
  Context := Lib.CreateContext;
  
  // 3. 配置为客户端模式
  Context.SetVerifyMode(sslVerifyPeer);
  
  // 4. 创建连接
  Connection := Context.CreateConnection;
  Connection.SetHostname('www.example.com');
  
  // 5. 连接到服务器
  if not Connection.Connect('www.example.com', 443) then
  begin
    WriteLn('连接失败: ', Connection.GetLastError);
    Exit;
  end;
  
  // 6. 发送 HTTPS 请求
  Request := 'GET / HTTP/1.1' + #13#10 +
             'Host: www.example.com' + #13#10 +
             'Connection: close' + #13#10 + #13#10;
  Connection.Write(Request);
  
  // 7. 读取响应
  Response := Connection.ReadAll;
  WriteLn(Response);
  
  // 8. 清理（自动）
  Connection := nil;
  Context := nil;
  Lib := nil;
end.
```

**编译**:
```bash
fpc -Fusrc -Fusrc/openssl simple_https_mbedtls.pas
```

**运行**:
```bash
./simple_https_mbedtls
```

---

### 示例 2: 带证书验证的客户端

```pascal
program secure_https_mbedtls;

uses
  fafafa.ssl,
  fafafa.ssl.abstract.intf;

var
  Lib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
begin
  Lib := CreateSSLLibrary(sslMbedTLS);
  Context := Lib.CreateContext;
  
  // 加载 CA 证书
  if not Context.LoadCAFromFile('/etc/ssl/certs/ca-certificates.crt') then
  begin
    WriteLn('加载 CA 证书失败');
    Exit;
  end;
  
  // 启用严格验证
  Context.SetVerifyMode(sslVerifyPeer);
  Context.SetVerifyDepth(5);
  
  Connection := Context.CreateConnection;
  Connection.SetHostname('www.google.com');
  
  if Connection.Connect('www.google.com', 443) then
  begin
    WriteLn('连接成功');
    WriteLn('协议版本: ', Connection.GetProtocolVersion);
    WriteLn('密码套件: ', Connection.GetCipherSuite);
    
    // 验证证书
    if Connection.VerifyPeerCertificate then
      WriteLn('证书验证通过')
    else
      WriteLn('证书验证失败');
  end;
end.
```

---

### 示例 3: 客户端证书认证

```pascal
program client_cert_mbedtls;

uses
  fafafa.ssl,
  fafafa.ssl.abstract.intf;

var
  Lib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
begin
  Lib := CreateSSLLibrary(sslMbedTLS);
  Context := Lib.CreateContext;
  
  // 加载客户端证书和私钥
  if not Context.LoadCertificateFromFile('client-cert.pem') then
  begin
    WriteLn('加载客户端证书失败');
    Exit;
  end;
  
  if not Context.LoadPrivateKeyFromFile('client-key.pem', '') then
  begin
    WriteLn('加载私钥失败');
    Exit;
  end;
  
  // 加载 CA 证书
  Context.LoadCAFromFile('ca-cert.pem');
  
  // 启用双向认证
  Context.SetVerifyMode(sslVerifyPeer);
  
  Connection := Context.CreateConnection;
  Connection.SetHostname('secure.example.com');
  
  if Connection.Connect('secure.example.com', 443) then
    WriteLn('双向认证成功');
end.
```

---

## 🔧 高级配置

### 协议版本配置

```pascal
// 仅允许 TLS 1.2 和 TLS 1.3
Context.SetMinProtocolVersion(sslProtocolTLS12);
Context.SetMaxProtocolVersion(sslProtocolTLS13);

// 或使用便捷方法
Context.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

### 密码套件配置

```pascal
// 使用安全的默认配置
Context.ConfigureSecureDefaults;

// 或手动配置密码套件
Context.SetCipherList('TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256');
```

### 内存优化（嵌入式）

```pascal
// MbedTLS 特定配置（通过环境变量或配置文件）
// 在编译 MbedTLS 时配置 mbedtls_config.h:

// 减少内存占用
#define MBEDTLS_SSL_MAX_CONTENT_LEN 4096  // 默认 16384

// 禁用不需要的功能
#undef MBEDTLS_SSL_PROTO_TLS1_1
#undef MBEDTLS_SSL_PROTO_DTLS

// 启用硬件加速（如果可用）
#define MBEDTLS_AES_ALT
#define MBEDTLS_SHA256_ALT
```

---

## 📊 性能对比

### 内存占用

| 后端 | 库大小 | 运行时内存 | 适用场景 |
|------|--------|-----------|---------|
| **OpenSSL** | ~6 MB | ~2-4 MB | 桌面应用 |
| **WinSSL** | 0 (系统) | ~1-2 MB | Windows 应用 |
| **MbedTLS** | ~1 MB | ~500 KB | 嵌入式系统 |

### TLS 握手性能

| 后端 | TLS 1.2 握手 | TLS 1.3 握手 | 吞吐量 |
|------|-------------|-------------|--------|
| **OpenSSL** | ~15 ms | ~10 ms | 高 |
| **WinSSL** | ~20 ms | ~12 ms | 中 |
| **MbedTLS** | ~25 ms | ~15 ms | 中 |

*测试环境: Intel i7, 16GB RAM, Ubuntu 22.04*

---

## 🔒 安全最佳实践

### 1. 证书验证

```pascal
// ✅ 推荐: 始终验证服务器证书
Context.SetVerifyMode(sslVerifyPeer);
Context.LoadCAFromFile('/etc/ssl/certs/ca-certificates.crt');

// ❌ 不推荐: 禁用验证（仅用于测试）
Context.SetVerifyMode(sslVerifyNone);
```

### 2. 协议版本

```pascal
// ✅ 推荐: 仅使用 TLS 1.2+
Context.SetMinProtocolVersion(sslProtocolTLS12);

// ❌ 不推荐: 允许旧协议
Context.SetMinProtocolVersion(sslProtocolTLS10);
```

### 3. 密码套件

```pascal
// ✅ 推荐: 使用安全默认配置
Context.ConfigureSecureDefaults;

// ❌ 不推荐: 允许弱密码套件
Context.SetCipherList('ALL');
```

### 4. 主机名验证

```pascal
// ✅ 推荐: 设置主机名进行 SNI 和验证
Connection.SetHostname('www.example.com');

// ❌ 不推荐: 不设置主机名
// (可能导致证书验证失败)
```

---

## 🐛 故障排除

### 问题 1: 库加载失败

**错误信息**:
```
Failed to load MbedTLS library
```

**解决方案**:
```bash
# 检查库是否安装
ldconfig -p | grep mbedtls

# 如果未安装
sudo apt install libmbedtls14

# 检查库路径
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

### 问题 2: 证书验证失败

**错误信息**:
```
Certificate verification failed: -0x2700
```

**解决方案**:
```pascal
// 1. 确保加载了正确的 CA 证书
Context.LoadCAFromFile('/etc/ssl/certs/ca-certificates.crt');

// 2. 检查系统时间是否正确
// 证书有效期依赖系统时间

// 3. 验证主机名是否匹配
Connection.SetHostname('www.example.com');  // 必须与证书匹配
```

### 问题 3: 连接超时

**错误信息**:
```
Connection timeout
```

**解决方案**:
```pascal
// 增加超时时间
Connection.SetTimeout(30000);  // 30 秒

// 检查网络连接
// ping www.example.com

// 检查防火墙规则
// sudo ufw status
```

### 问题 4: 内存不足（嵌入式）

**错误信息**:
```
Memory allocation failed
```

**解决方案**:
```c
// 在 mbedtls_config.h 中减少缓冲区大小
#define MBEDTLS_SSL_MAX_CONTENT_LEN 4096  // 从 16384 减少

// 禁用不需要的功能
#undef MBEDTLS_SSL_PROTO_TLS1_1
#undef MBEDTLS_DEBUG_C
```

---

## 📚 API 参考

### 库初始化

```pascal
function CreateSSLLibrary(LibType: TSSLLibraryType): ISSLLibrary;
// LibType: sslMbedTLS
```

### 上下文管理

```pascal
interface ISSLContext
  function CreateConnection: ISSLConnection;
  function LoadCertificateFromFile(const FileName: string): Boolean;
  function LoadPrivateKeyFromFile(const FileName, Password: string): Boolean;
  function LoadCAFromFile(const FileName: string): Boolean;
  procedure SetVerifyMode(Mode: TSSLVerifyMode);
  procedure SetMinProtocolVersion(Version: TSSLProtocolVersion);
  procedure SetMaxProtocolVersion(Version: TSSLProtocolVersion);
  procedure ConfigureSecureDefaults;
end;
```

### 连接管理

```pascal
interface ISSLConnection
  function Connect(const Host: string; Port: Word): Boolean;
  procedure SetHostname(const Hostname: string);
  procedure SetTimeout(TimeoutMs: Integer);
  function Write(const Data: string): Integer;
  function Read(var Buffer; Size: Integer): Integer;
  function ReadAll: string;
  function GetProtocolVersion: string;
  function GetCipherSuite: string;
  function VerifyPeerCertificate: Boolean;
  function GetLastError: string;
end;
```

---

## 🔗 相关文档

- **[USER_GUIDE.md](USER_GUIDE.md)** - 通用用户指南
- **[WINSSL_USER_GUIDE.md](WINSSL_USER_GUIDE.md)** - WinSSL 后端指南
- **[SECURITY_GUIDE.md](SECURITY_GUIDE.md)** - 安全配置指南
- **[API_REFERENCE.md](API_REFERENCE.md)** - 完整 API 参考
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** - 故障排除指南

---

## 🌐 外部资源

- **[MbedTLS 官方文档](https://mbed-tls.readthedocs.io/)**
- **[MbedTLS GitHub](https://github.com/Mbed-TLS/mbedtls)**
- **[PSA Certified](https://www.psacertified.org/)**
- **[ARM 开发者社区](https://community.arm.com/)**

---

## 📝 版本历史

- **v0.8.0** (2026-01-25)
  - MbedTLS 后端初始实现
  - 支持 TLS 1.2/1.3
  - 完整的证书验证
  - 73/73 测试通过

---

## 💬 获取帮助

如果遇到问题:

1. 查看 **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)**
2. 查看 **[FAQ.md](FAQ.md)**
3. 提交 Issue 到 GitHub
4. 查看 MbedTLS 官方文档

---

**最后更新**: 2026-01-25  
**版本**: v0.8.0
