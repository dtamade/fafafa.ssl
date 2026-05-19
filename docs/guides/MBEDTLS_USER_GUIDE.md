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

- 上游 Mbed TLS 生态有安全认证 / PSA / 商业支持路线
- 但 fafafa.ssl 当前 MbedTLS backend 仍以当前 shipped public surface 为准
- 当前 `SupportsFIPSMode=False`：不要把上游认证能力直接外推成 fafafa.ssl 当前 backend truth

#### 4. ✅ 统一 API

MbedTLS 与其它 backend 共享统一核心接口，但具体 published capability 仍以后端的 `ISSLLibrary.GetCapabilities` 为准。

当前需要特别记住的 MbedTLS truth：

- 当前 `SupportsCallbacks=False`：verify / password / info callback 的 non-nil assignment 会 fail-closed `unsupported`。
- 当前 `SupportsFIPSMode=False`：不要把上游 Mbed TLS 的认证/商业版本能力当成 fafafa.ssl 当前 backend truth。
- 当前不发布 `ISSLEarlyDataContext / ISSLEarlyDataConnection` public surface；0-RTT 应视为 current capability none。
- 当前 `SupportsPKCS12=False`：没有 shipped PKCS#12 bundle create / parse / import surface。

```pascal
// 核心调用形状相近，但 capability 需要按 backend 重新核对
{$IFDEF EMBEDDED}
Lib := TSSLFactory.GetLibraryInstance(sslMbedTLS);   // 嵌入式: 轻量级
{$ELSE}
Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);   // 桌面: 功能完整
{$ENDIF}

Context := Lib.CreateContext(sslCtxClient);
Context.LoadCertificate('cert.pem');
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
  fafafa.ssl;

var
  Lib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  ClientConn: ISSLClientConnection;
  Request, Response: string;
  Socket: THandle;
begin
  // 1. 初始化 MbedTLS 库
  Lib := TSSLFactory.GetLibraryInstance(sslMbedTLS);
  
  // 2. 创建上下文
  Context := Lib.CreateContext(sslCtxClient);
  
  // 3. 配置为客户端模式
  Context.SetVerifyMode([sslVerifyPeer]);
  Context.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');
  
  // 4. 创建连接（示意：Socket 代表已建立的 TCP 连接）
  Socket := { already-connected TCP socket };
  Connection := Context.CreateConnection(Socket);
  if Supports(Connection, ISSLClientConnection, ClientConn) then
    ClientConn.SetServerName('www.example.com');
  
  // 5. 连接到服务器
  if not Connection.Connect then
  begin
    WriteLn('连接失败: ', Connection.GetLastErrorString);
    Exit;
  end;
  
  // 6. 发送 HTTPS 请求
  Request := 'GET / HTTP/1.1' + #13#10 +
             'Host: www.example.com' + #13#10 +
             'Connection: close' + #13#10 + #13#10;
  Connection.WriteString(Request);
  
  // 7. 读取响应
  if Connection.ReadString(Response) then
    WriteLn(Response);
  
  // 8. 清理（自动）
  Connection := nil;
  Context := nil;
  Lib := nil;
end.
```

**编译**:
```bash
fpc -Fusrc simple_https_mbedtls.pas
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
  fafafa.ssl;

var
  Lib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  ClientConn: ISSLClientConnection;
  Socket: THandle;
begin
  Lib := TSSLFactory.GetLibraryInstance(sslMbedTLS);
  Context := Lib.CreateContext(sslCtxClient);
  
  // 加载 CA 证书
  Context.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');
  
  // 启用严格验证
  Context.SetVerifyMode([sslVerifyPeer]);
  Context.SetVerifyDepth(5);
  
  Socket := { already-connected TCP socket };
  Connection := Context.CreateConnection(Socket);
  if Supports(Connection, ISSLClientConnection, ClientConn) then
    ClientConn.SetServerName('www.google.com');
  
  if Connection.Connect then
  begin
    WriteLn('连接成功');
    WriteLn('协议版本: ', Connection.GetProtocolVersion);
    WriteLn('密码套件: ', Connection.GetCipherName);
    
    // 对端验证结果请通过 connection info / verify-result API 获取
    WriteLn('对端验证已在握手阶段按 verify mode 执行');
  end;
end.
```

---

### 示例 3: 客户端证书认证

```pascal
program client_cert_mbedtls;

uses
  fafafa.ssl;

var
  Lib: ISSLLibrary;
  Context: ISSLContext;
  Connection: ISSLConnection;
  ClientConn: ISSLClientConnection;
  Socket: THandle;
begin
  Lib := TSSLFactory.GetLibraryInstance(sslMbedTLS);
  Context := Lib.CreateContext(sslCtxClient);
  
  // 加载客户端证书和私钥
  Context.LoadCertificate('client-cert.pem');
  Context.LoadPrivateKey('client-key.pem', '');
  
  // 加载 CA 证书
  Context.LoadCAFile('ca-cert.pem');
  
  // 启用双向认证
  Context.SetVerifyMode([sslVerifyPeer]);
  
  Socket := { already-connected TCP socket };
  Connection := Context.CreateConnection(Socket);
  if Supports(Connection, ISSLClientConnection, ClientConn) then
    ClientConn.SetServerName('secure.example.com');
  
  if Connection.Connect then
    WriteLn('双向认证成功');
end.
```

---

## 🔧 高级配置

### 协议版本配置

```pascal
// 仅允许 TLS 1.2 和 TLS 1.3
Context.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

### 密码套件配置

```pascal
// 推荐: 用 builder 生成安全默认配置
Context := TSSLContextBuilder.Create
  .WithBackend(sslMbedTLS)
  .WithSafeDefaults
  .BuildClient;
```

当前 `SupportsCustomCipherSuites=False`：`SetCipherList` / `SetCipherSuites` 的 custom non-default assignment 会 fail-closed `unsupported`；如需安全基线，请使用 builder / default-context path。

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
Context.SetVerifyMode([sslVerifyPeer]);
Context.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');

// ❌ 不推荐: 禁用验证（仅用于测试）
Context.SetVerifyMode([sslVerifyNone]);
```

### 2. 协议版本

```pascal
// ✅ 推荐: 仅使用 TLS 1.2+
Context.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

// ❌ 不推荐: 允许旧协议
Context.SetProtocolVersions([sslProtocolTLS10, sslProtocolTLS12]);
```

### 3. 密码套件

```pascal
// ✅ 推荐: 使用 builder 的安全默认配置
Context := TSSLContextBuilder.Create
  .WithBackend(sslMbedTLS)
  .WithSafeDefaults
  .BuildClient;
```

不要尝试通过 `SetCipherList('ALL')` 放宽当前 MbedTLS backend 的 cipher policy；custom non-default override 当前不会被发布。

### 4. 主机名验证

```pascal
// ✅ 推荐: 设置主机名进行 SNI 和验证
if Supports(Connection, ISSLClientConnection, ClientConn) then
  ClientConn.SetServerName('www.example.com');

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
Context.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');

// 2. 检查系统时间是否正确
// 证书有效期依赖系统时间

// 3. 验证主机名是否匹配
if Supports(Connection, ISSLClientConnection, ClientConn) then
  ClientConn.SetServerName('www.example.com');  // 必须与证书匹配
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
class function TSSLFactory.GetLibraryInstance(
  ALibType: TSSLLibraryType = sslAutoDetect
): ISSLLibrary;
// MbedTLS 指南中显式使用 sslMbedTLS
```

### 上下文管理

```pascal
interface ISSLContext
  procedure LoadCertificate(const AFileName: string); overload;
  procedure LoadPrivateKey(const AFileName: string; const APassword: string = ''); overload;
  procedure LoadCAFile(const AFileName: string);
  procedure SetVerifyMode(AMode: TSSLVerifyModes);
  procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
  function CreateConnection(ASocket: THandle): ISSLConnection; overload;
end;
```

### 连接管理

```pascal
interface ISSLConnection
  function Connect: Boolean;
  function WriteString(const AStr: string): Boolean;
  function ReadString(out AStr: string): Boolean;
  function GetProtocolVersion: string;
  function GetCipherName: string;
  function GetLastErrorString: string;
end;

interface ISSLClientConnection
  procedure SetServerName(const AServerName: string);
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
