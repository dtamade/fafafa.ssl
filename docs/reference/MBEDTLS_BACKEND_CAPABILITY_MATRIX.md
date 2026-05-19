# MbedTLS 后端能力矩阵

> **Batch**: B66
> **Status**: draft
> **Created**: 2026-02-07
> **Backend**: MbedTLS (Mbed TLS / PolarSSL)

## 概述

MbedTLS 是一个轻量级、可移植的 TLS 库，特别适合嵌入式系统和资源受限环境。fafafa.ssl 提供 MbedTLS 后端作为 OpenSSL 的替代选项。

> 这份矩阵描述的是 **fafafa.ssl 当前 published public surface**，不直接等于上游 Mbed TLS 原始库理论能力。

## 后端标识

```pascal
uses fafafa.ssl.base;

// 后端枚举值
sslMbedTLS  // TSSLBackend.sslMbedTLS

// 使用示例
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslMbedTLS)
  .WithCAFile('/etc/ssl/certs/ca-certificates.crt')
  .BuildClient;
```

## 能力矩阵

### TLS 协议支持

| 功能 | 支持状态 | 说明 |
|------|----------|------|
| TLS 1.0 | ❌ 当前 capability 不发布 | 当前 MbedTLS runtime path 以 TLS 1.2+ 为起点；`sslProtocolTLS10` 当前返回 `False` |
| TLS 1.1 | ❌ 当前 capability 不发布 | 当前 MbedTLS runtime path 以 TLS 1.2+ 为起点；`sslProtocolTLS11` 当前返回 `False` |
| TLS 1.2 | ✅ 支持 | 默认启用，推荐 |
| TLS 1.3 | ✅ 支持 | MbedTLS 3.x 支持 |
| DTLS 1.0 | ❌ 当前 capability 不发布 | 当前 `SupportsDTLS=False`；`sslProtocolDTLS10` 当前返回 `False` |
| DTLS 1.2 | ❌ 当前 capability 不发布 | 当前 `SupportsDTLS=False`；`sslProtocolDTLS12` 当前返回 `False` |

### 密码套件

| 类别 | 支持状态 | 说明 |
|------|----------|------|
| AES-GCM | ✅ 支持 | 推荐 |
| AES-CCM | ✅ 支持 | 适合嵌入式 |
| AES-CBC | ✅ 支持 | 兼容性 |
| ChaCha20-Poly1305 | ✅ 支持 | MbedTLS 2.x+ |
| 3DES | ⚠️ 可选 | 不推荐，需编译时启用 |
| RC4 | ❌ 不支持 | 已移除 |

### 密钥交换

| 算法 | 支持状态 | 说明 |
|------|----------|------|
| RSA | ✅ 支持 | 兼容性 |
| DHE | ✅ 支持 | 前向保密 |
| ECDHE | ✅ 支持 | 推荐 |
| PSK | ✅ 支持 | 预共享密钥 |
| ECDHE-PSK | ✅ 支持 | 组合模式 |

### 签名算法

| 算法 | 支持状态 | 说明 |
|------|----------|------|
| RSA-PKCS1 | ✅ 支持 | 兼容性 |
| RSA-PSS | ✅ 支持 | 推荐 |
| ECDSA | ✅ 支持 | 推荐 |
| Ed25519 | ⚠️ 部分 | MbedTLS 3.x |

### 椭圆曲线

| 曲线 | 支持状态 | 说明 |
|------|----------|------|
| secp256r1 (P-256) | ✅ 支持 | 推荐 |
| secp384r1 (P-384) | ✅ 支持 | 高安全性 |
| secp521r1 (P-521) | ✅ 支持 | 最高安全性 |
| x25519 | ✅ 支持 | MbedTLS 2.x+ |
| brainpool | ⚠️ 可选 | 需编译时启用 |

### 证书功能

| 功能 | 支持状态 | 说明 |
|------|----------|------|
| X.509 解析 | ✅ 支持 | 完整支持 |
| 证书链验证 | ✅ 支持 | 完整支持 |
| CRL 检查 | ✅ 支持 | 本地 CRL |
| OCSP | ❌ 当前 capability 不发布 | 当前 backend 没有 shipped online OCSP verification public surface；如需相关 revocation workflow，需由应用层在 fafafa.ssl 已发布 surface 之外自行实现 |
| OCSP Stapling | ❌ 当前 capability 不发布 | 当前 backend 不暴露 `ISSLOCSPStapling` / `ISSLServerOCSPStaplingContext`；`server_ocsp_stapled_response_file` 配置会在 builder 侧 fail-fast |
| 证书固定 | ✅ 支持 | 使用 context pinning API（AddCertificatePin / SetCertificatePinningEnabled），不是 callback surface |
| SNI | ✅ 支持 | 客户端/服务器 |

### Session 管理

| 功能 | 支持状态 | 说明 |
|------|----------|------|
| Session 复用 | ✅ 支持 | 完整支持 |
| Session Ticket | ✅ 支持 | TLS 1.2+ |
| Session Cache | ✅ 支持 | 内置缓存 |
| 0-RTT | ❌ 当前 capability 不发布 | 当前 backend 不暴露 ISSLEarlyDataContext / ISSLEarlyDataConnection public surface |

### 高级功能

| 功能 | 支持状态 | 说明 |
|------|----------|------|
| ALPN | ✅ 支持 | 完整支持 |
| 重协商 | ⚠️ 可选 | 安全重协商 |
| 客户端证书 | ✅ 支持 | 双向 TLS |
| 自定义 I/O | ❌ 当前 public callback surface 不发布 | 当前 transport path 仅使用内置 socket/stream BIO wiring，不提供 caller-supplied I/O callback seam |
| 异步操作 | ⚠️ 部分 | 非阻塞 I/O |

## 与 OpenSSL 对比

| 特性 | MbedTLS | OpenSSL |
|------|---------|---------|
| 代码大小 | 小 (~100KB) | 大 (~2MB) |
| 内存占用 | 低 | 中等 |
| 功能完整性 | 核心 TLS | 完整密码学 |
| FIPS 认证 | 无 | 有 |
| 许可证 | Apache 2.0 | Apache 2.0 |
| 嵌入式适用 | 优秀 | 一般 |
| 文档质量 | 优秀 | 良好 |

## 平台支持

| 平台 | 支持状态 | 库文件 |
|------|----------|--------|
| Linux x86_64 | ✅ 支持 | libmbedtls.so, libmbedcrypto.so, libmbedx509.so |
| Linux ARM | ✅ 支持 | 同上 |
| macOS | ✅ 支持 | libmbedtls.dylib, ... |
| Windows | ✅ 支持 | mbedtls.dll, ... |
| FreeBSD | ✅ 支持 | libmbedtls.so, ... |

## 安装指南

### Linux (Debian/Ubuntu)

```bash
sudo apt install libmbedtls-dev
```

### Linux (Fedora/RHEL)

```bash
sudo dnf install mbedtls-devel
```

### macOS

```bash
brew install mbedtls
```

### Windows

从 [MbedTLS 官网](https://tls.mbed.org/) 下载预编译库或自行编译。

## 使用示例

### 基本 TLS 客户端

```pascal
program mbedtls_client;

uses
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  // 创建 MbedTLS 上下文
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslMbedTLS)
    .WithTLS12And13
    .WithVerifyPeer
    .WithCAFile('/etc/ssl/certs/ca-certificates.crt')
    .BuildClient;

  // 创建连接
  Conn := Ctx.CreateConnection(Socket);
  (Conn as ISSLClientConnection).SetServerName('example.com');

  if Conn.Connect then
  begin
    WriteLn('Connected with: ', Conn.GetCipherName);
    // 读写数据...
    Conn.Shutdown;
  end;
end.
```

### 传输层说明

当前 MbedTLS backend 通过内部 `mbedtls_ssl_set_bio` 把 socket / stream 连接接到 TLS 层。

- 已发布给调用方的 transport public surface：
  - `ISSLContext.CreateConnection(ASocket: THandle)`
  - `ISSLContext.CreateConnection(AStream: TStream)`
- 当前没有 published caller-supplied custom I/O callback seam

## 限制与注意事项

1. **OCSP / OCSP Stapling**: 当前 backend 不发布 online OCSP 或 stapled-response public capability；如需相关 revocation workflow，需在 fafafa.ssl 已发布 surface 之外自行实现
2. **硬件加速**: 需要编译时启用特定平台的硬件加速
3. **FIPS 模式**: 当前 `SupportsFIPSMode=False`，不要把上游特定商业/认证版本能力当成 fafafa.ssl 当前 backend truth
4. **API 差异**: 与 OpenSSL backend 共享统一核心接口，但 published capability 仍然 backend-specific

## 相关文档

- `src/fafafa.ssl.mbedtls.*.pas` - MbedTLS 后端实现
- `tests/test_mbedtls_framework.pas` - MbedTLS 测试
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md` - 总体能力矩阵
