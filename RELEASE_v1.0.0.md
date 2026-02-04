# fafafa.ssl v1.0.0 发布说明

**发布日期**: 2026-02-05

---

## 概述

fafafa.ssl v1.0.0 是该项目的首个正式发布版本，提供企业级 SSL/TLS 加密能力，
专为 FreePascal 开发者设计。

### 项目规模

| 指标 | 数值 |
|------|------|
| 源文件 | 160 个 |
| 代码行数 | 95,143 行 |
| 测试文件 | 415 个 |
| 测试通过率 | 100% |
| 示例程序 | 57 个 |
| TODO 残留 | 0 个 |

---

## 主要功能

### 多后端支持

- **OpenSSL 后端** - 支持 1.1.1+ 和 3.0+，跨平台
- **WinSSL 后端** - Windows 原生 Schannel，零依赖部署
- **MbedTLS 后端** - 可选，轻量级嵌入式场景
- **WolfSSL 后端** - 可选，高安全场景

### TLS 协议支持

- TLS 1.2 / TLS 1.3 自动协商
- 强密码套件（AES-256-GCM, CHACHA20-POLY1305）
- SNI (Server Name Indication) 支持
- ALPN (Application-Layer Protocol Negotiation) 支持

### 证书管理

- X.509 证书解析与验证
- 证书生成（自签名、CA 签发）
- CRL/OCSP 吊销检查
- 证书钉扎 (Certificate Pinning)
- 系统 CA 自动加载

### PKCS#11 硬件安全模块

- 动态加载 PKCS#11 库
- 支持 SoftHSM2、YubiKey 等
- PIN 回调机制
- 私钥加载与签名

### DANE/DNSSEC

- TLSA 记录查询与验证
- 可选 ldns 库集成
- DNSSEC 签名验证

### 性能优化

- **无锁环形缓冲区** - 16M+ ops/s
- **三级内存池** - 100% 命中率
- **分片会话缓存** - 8-16x 并发提升
- **会话复用** - 70-90% 握手性能提升

---

## 安装说明

### 系统要求

| 平台 | 最低要求 |
|------|---------|
| FreePascal | 3.2.0+ |
| OpenSSL | 1.1.1+ 或 3.0+ |
| Windows | 10/11, Server 2016+ |
| Linux | Ubuntu 20.04+, Debian 11+ |
| macOS | 10.15+ |

### 安装步骤

#### Ubuntu/Debian

```bash
# 安装依赖
sudo apt-get install libssl-dev fpc

# 下载源码
git clone https://github.com/dtamade/fafafa.ssl.git
cd fafafa.ssl

# 编译示例
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src examples/hello_ssl.pas
```

#### macOS

```bash
# 安装依赖
brew install fpc openssl@3

# 下载源码
git clone https://github.com/dtamade/fafafa.ssl.git
cd fafafa.ssl

# 编译（指定 OpenSSL 路径）
export LIBRARY_PATH=/opt/homebrew/opt/openssl@3/lib
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src examples/hello_ssl.pas
```

#### Windows

```powershell
# 安装 FreePascal
choco install freepascal

# 下载源码
git clone https://github.com/dtamade/fafafa.ssl.git
cd fafafa.ssl

# 使用 WinSSL 后端（无需 OpenSSL）
fpc -B -Mobjfpc -Sh -Fu.\src -Fi.\src -dUSE_WINSSL examples\hello_ssl.pas
```

---

## 快速开始

### Hello TLS

```pascal
program HelloTLS;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory,
  fafafa.ssl.base;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  YourSocket: THandle;  // 你的 TCP socket
begin
  // 1. 创建上下文
  Ctx := TSSLFactory.CreateContext(sslCtxClient);

  // 2. 创建连接
  Conn := Ctx.CreateConnection(YourSocket);

  // 3. 设置服务器名称（SNI）
  (Conn as ISSLClientConnection).SetServerName('www.example.com');

  // 4. SSL 握手
  if Conn.Connect then
  begin
    WriteLn('TLS 连接成功');
    WriteLn('协议: ', Conn.GetProtocolVersion);
    WriteLn('密码套件: ', Conn.GetCipherName);
  end;

  // 5. 关闭
  Conn.Shutdown;
end.
```

### 更多示例

- `examples/01_tls_client.pas` - TLS 客户端
- `examples/02_generate_certificate.pas` - 证书生成
- `examples/03_file_encryption.pas` - 文件加密
- `examples/04_https_rest_client.pas` - HTTPS REST 客户端
- `examples/05_https_server.pas` - HTTPS 服务器
- `examples/hello_ssl.pas` - 最简示例

---

## API 概览

### 上下文创建

```pascal
// 使用工厂创建
Ctx := TSSLFactory.CreateContext(sslCtxClient);

// 使用 Builder 模式
Ctx := TSSLContextBuilder.Create
  .WithTLS12And13
  .WithVerifyPeer
  .WithSystemRoots
  .BuildClient;
```

### 加密操作

```pascal
uses fafafa.ssl.crypto.utils;

// SHA-256
Hash := TCryptoUtils.SHA256Hex('Hello World');

// AES-256-GCM
Encrypted := TCryptoUtils.AES_GCM_Encrypt(Data, Key, IV);
Decrypted := TCryptoUtils.AES_GCM_Decrypt(Encrypted, Key, IV);

// 安全随机数
Random := TCryptoUtils.SecureRandom(32);
```

### 证书操作

```pascal
uses fafafa.ssl.cert.builder;

// 创建自签名证书
KeyPair := TCertificateBuilder.Create
  .WithCommonName('localhost')
  .WithOrganization('My Company')
  .ValidFor(365)
  .WithRSAKey(2048)
  .SelfSigned;

KeyPair.SaveToFiles('server.crt', 'server.key');
```

---

## 已知限制

1. **MbedTLS/WolfSSL 后端** - 需要手动编译并链接对应库
2. **OCSP Stapling** - 仅 OpenSSL 后端完全支持
3. **PKCS#11** - 需要安装对应的 PKCS#11 模块
4. **DANE/DNSSEC** - 需要可选安装 ldns 库

---

## 从旧版本升级

### 从 v0.8.x 升级

v1.0.0 完全向后兼容 v0.8.x，无需修改代码即可升级。

新增功能可选使用：

```pascal
// 使用 PKCS#11
uses fafafa.ssl.pkcs11.engine;
Engine := TPKCS11Engine.Create('/path/to/pkcs11.so');

// 使用无锁缓冲区
uses fafafa.ssl.lockfree.ringbuffer;
Buffer := TLockFreeRingBuffer.Create(4096);
```

---

## 测试验证

```bash
# 运行全部测试
./ci_pipeline.sh test

# 运行性能基准
./ci_pipeline.sh bench

# 完整 CI 流程
./ci_pipeline.sh all
```

---

## 致谢

感谢所有为 fafafa.ssl 项目做出贡献的开发者。

特别感谢：
- [OpenSSL Project](https://www.openssl.org/) - 加密库
- [Free Pascal](https://www.freepascal.org/) - 编译器
- [ldns](https://www.nlnetlabs.nl/projects/ldns/) - DNS 库

---

## 支持

- **文档**: [docs/](docs/)
- **示例**: [examples/](examples/)
- **Issues**: https://github.com/dtamade/fafafa.ssl/issues
- **Discussions**: https://github.com/dtamade/fafafa.ssl/discussions

---

## 许可证

MIT License - 详见 [LICENSE](LICENSE)

---

**fafafa.ssl v1.0.0** - Enterprise-grade SSL/TLS Library for FreePascal

Made with care by the fafafa.ssl team
