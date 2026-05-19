# WinSSL (Schannel) 后端能力矩阵

> **Batch**: B67
> **Status**: draft
> **Created**: 2026-02-07
> **Backend**: WinSSL (Windows Schannel / SChannel)

## 概述

WinSSL 是 fafafa.ssl 对 Windows 原生 TLS 实现（Schannel）的封装。使用 WinSSL 后端无需安装额外的 DLL，直接使用 Windows 系统内置的 TLS 功能。

## 后端标识

```pascal
uses fafafa.ssl.base;

// 后端枚举值
sslWinSSL  // TSSLBackend.sslWinSSL

// 使用示例
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslWinSSL)
  .BuildClient;
```

## 能力矩阵

### TLS 协议支持

| 功能     | Windows 10+ | Windows 8.1 | Windows 7 | 说明             |
| -------- | ----------- | ----------- | --------- | ---------------- |
| TLS 1.0  | ⚠️ 禁用     | ✅ 支持     | ✅ 支持   | 默认禁用，不推荐 |
| TLS 1.1  | ⚠️ 禁用     | ✅ 支持     | ✅ 支持   | 默认禁用，不推荐 |
| TLS 1.2  | ✅ 支持     | ✅ 支持     | ⚠️ 更新后 | 推荐             |
| TLS 1.3  | ✅ 支持     | ❌ 不支持   | ❌ 不支持 | Windows 10 1903+ |
| DTLS 1.0 | ✅ 支持     | ✅ 支持     | ⚠️ 部分   |                  |
| DTLS 1.2 | ✅ 支持     | ⚠️ 部分     | ❌ 不支持 |                  |

### 密码套件

| 类别              | 支持状态 | 说明             |
| ----------------- | -------- | ---------------- |
| AES-GCM           | ✅ 支持  | 推荐             |
| AES-CBC           | ✅ 支持  | 兼容性           |
| ChaCha20-Poly1305 | ⚠️ 部分  | Windows 10 1903+ |
| 3DES              | ⚠️ 可选  | 不推荐           |
| RC4               | ❌ 禁用  | 安全原因         |

| Custom cipher configuration | ❌ 不支持 | 当前由系统 Schannel policy / Windows cipher order 决定；custom non-default `SetCipherList` / `SetCipherSuites` 会 fail-closed 为 unsupported |

### 密钥交换

| 算法  | 支持状态  | 说明          |
| ----- | --------- | ------------- |
| RSA   | ✅ 支持   | 兼容性        |
| DHE   | ✅ 支持   | 前向保密      |
| ECDHE | ✅ 支持   | 推荐          |
| PSK   | ❌ 不支持 | Schannel 限制 |

### 签名算法

| 算法      | 支持状态  | 说明          |
| --------- | --------- | ------------- |
| RSA-PKCS1 | ✅ 支持   | 兼容性        |
| RSA-PSS   | ✅ 支持   | Windows 10+   |
| ECDSA     | ✅ 支持   | 推荐          |
| Ed25519   | ❌ 不支持 | Schannel 限制 |

### 椭圆曲线

| 曲线              | 支持状态 | 说明             |
| ----------------- | -------- | ---------------- |
| secp256r1 (P-256) | ✅ 支持  | 推荐             |
| secp384r1 (P-384) | ✅ 支持  | 高安全性         |
| secp521r1 (P-521) | ✅ 支持  | 最高安全性       |
| x25519            | ⚠️ 部分  | Windows 10 1903+ |

### 证书功能

| 功能          | 支持状态 | 说明                                                                                   |
| ------------- | -------- | -------------------------------------------------------------------------------------- |
| X.509 解析    | ✅ 支持  | 通过 CryptoAPI                                                                         |
| 证书链验证    | ✅ 支持  | 系统证书存储                                                                           |
| CRL 检查      | ✅ 支持  | 自动下载                                                                               |
| OCSP          | ✅ 支持  | 自动检查                                                                               |
| OCSP Stapling | ⚠️ 部分  | 服务器端（Schannel 系统级行为；fafafa.ssl 封装层不暴露 ISSLServerOCSPStaplingContext） |
| 证书固定      | ✅ 支持  | 通过回调                                                                               |
| SNI           | ✅ 支持  | 客户端/服务器                                                                          |
| 系统证书存储  | ✅ 支持  | 原生集成                                                                               |

### Session 管理

| 功能           | 支持状态  | 说明                                                                                                               |
| -------------- | --------- | ------------------------------------------------------------------------------------------------------------------ |
| Session 复用   | ⚠️ 实验性 | public surface 存在；当前 dedicated Windows CI runtime truth 为 `observed_reuse=false` / `session_configured=true` |
| Session Ticket | ⚠️ 实验性 | Schannel surface 存在，但 fafafa.ssl 尚未在 dedicated Windows proof 中观测到真实 resumed handshake                 |
| Session Cache  | ✅ 支持   | 系统管理；`SessionCacheSupport=sslSupportStable` 代表 context-level cache/control surface 已发布且已接线，不等于当前已 runtime-proven 的 resumed handshake |
| 0-RTT          | ⚠️ 部分   | TLS 1.3（Schannel 有限支持；fafafa.ssl 封装层不暴露 ISSLEarlyDataContext）                                         |

> 当前 dedicated Windows CI runtime truth 以 run `26037518301` 为准：
> `observed_reuse=false`，`session_configured=true`。
> 由于 canonical shared path 当前继续撤下 live `SECPKG_ATTR_SESSION_INFO` probe 以避免 Windows AV，`observed_reuse` 当前应按 conservative public truth 理解；更深 native evidence 仍需看 opt-in isolated native probe 输出的 `native_observed_reuse` / `native_probe_succeeded`。
> 因此 WinSSL `session resumption / tickets` 当前只能作为实验性 public surface 使用，不能再写成“完整支持”。

### 高级功能

| 功能                            | 支持状态                  | 说明                                                                                                                     |
| ------------------------------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| ALPN                            | ✅ 支持                   | Windows 8.1+                                                                                                             |
| 重协商                          | ✅ 支持                   | 安全重协商                                                                                                               |
| 客户端证书                      | ✅ 支持                   | 双向 TLS                                                                                                                 |
| Context callbacks               | ⚠️ 部分                   | 当前仅 verify/info runtime path 已发布；password callback 仍为 unsupported                                               |
| Password-protected private keys | ⚠️ 部分                   | 当前仅 password-protected PFX/P12 import path 已发布；PEM private-key password path 仍为 unsupported                     |
| DER / PKCS#8 private keys       | ❌ 当前 capability 不发布 | 目前没有 shipped bare DER / PKCS#8 private-key load path；请改用 PFX/P12 或 OpenSSL backend                              |
| 智能卡 / PKCS#11                | ❌ 当前 capability 不发布 | Windows 平台底层可接触硬件密钥，但 fafafa.ssl 当前 WinSSL backend 没有 shipped PKCS#11 URI / smart-card 私钥加载 surface |
| TPM                             | ❌ 当前 capability 不发布 | Schannel/CNG 的平台潜在能力不等于 fafafa.ssl 已发布 TPM loading/runtime contract                                         |

> `SupportsCallbacks=True` 在 WinSSL 上当前是 coarse-grained publication flag：
> verify/info callback 已发布，password callback 仍未接入 runtime，non-nil assignment 会 fail-closed 为 unsupported。

## 与 OpenSSL 对比

| 特性                     | WinSSL                                     | OpenSSL                       |
| ------------------------ | ------------------------------------------ | ----------------------------- |
| 安装依赖                 | 无                                         | 需要 DLL                      |
| 系统集成                 | 原生                                       | 独立                          |
| 证书存储                 | 系统存储                                   | 文件/内存                     |
| FIPS policy / capability | 系统策略检测（不作为当前 capability 发布） | 库级 capability（需专门构建） |
| 跨平台                   | 仅 Windows                                 | 全平台                        |
| 更新方式                 | Windows Update                             | 手动更新                      |
| 功能完整性               | TLS 核心                                   | 完整密码学                    |

## 平台支持

| Windows 版本        | 支持状态 | TLS 1.3 | 说明    |
| ------------------- | -------- | ------- | ------- |
| Windows 11          | ✅ 完整  | ✅      | 推荐    |
| Windows 10 1903+    | ✅ 完整  | ✅      | 推荐    |
| Windows 10 1809-    | ✅ 支持  | ❌      | TLS 1.2 |
| Windows 8.1         | ✅ 支持  | ❌      | TLS 1.2 |
| Windows 7 SP1       | ⚠️ 部分  | ❌      | 需更新  |
| Windows Server 2022 | ✅ 完整  | ✅      | 推荐    |
| Windows Server 2019 | ✅ 支持  | ⚠️      | 需更新  |
| Windows Server 2016 | ✅ 支持  | ❌      | TLS 1.2 |

## 使用示例

### 基本 TLS 客户端

```pascal
program winssl_client;

uses
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  // 创建 WinSSL 上下文（无需安装 OpenSSL）
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL)
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots  // 使用 Windows 证书存储
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

### Session 复用

```pascal
program winssl_session_reuse;

var
  Ctx: ISSLContext;
  Conn1, Conn2: ISSLConnection;
  Resumption1, Resumption2: ISSLSessionResumption;
  Session: ISSLSession;
begin
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL)
    .WithTLS12And13
    .BuildClient;

  // 第一次连接
  Conn1 := Ctx.CreateConnection(Socket1);
  (Conn1 as ISSLClientConnection).SetServerName('api.example.com');
  if Conn1.Connect and Supports(Conn1, ISSLSessionResumption, Resumption1) then
  begin
    Session := Resumption1.GetSession;  // 保存 Session
    Conn1.Shutdown;
  end;

  // 第二次连接（尝试复用 Session）
  Conn2 := Ctx.CreateConnection(Socket2);
  (Conn2 as ISSLClientConnection).SetServerName('api.example.com');
  if Supports(Conn2, ISSLSessionResumption, Resumption2) and Assigned(Session) then
    Resumption2.SetSession(Session);  // 保存 compatibility metadata；Schannel reconnect 仍主要取决于 target name + credential handle

  if Conn2.Connect and Supports(Conn2, ISSLSessionResumption, Resumption2) then
  begin
    if Resumption2.IsSessionReused then
      WriteLn('当前连接命中了 resumed handshake')
    else
      WriteLn('当前 dedicated Windows CI runtime truth 仍可能是 observed_reuse=false / session_configured=true');
  end;
end.
```

> 注意：按当前实现，WinSSL 的 `ISSLSessionResumption.SetSession(...)` 更接近
> compatibility metadata surface；client-side reconnect/cache lookup 仍主要取决于
> 相同的 `target name` 与相同的 context-level `credential handle`，而不是显式的
> native session-handle 注入。

### 使用系统证书存储

```pascal
program winssl_certstore;

uses
  fafafa.ssl.winssl.certstore;

var
  Store: ISSLCertificateStore;
  Cert: ISSLCertificate;
begin
  // 打开系统证书存储
  Store := TWinSSLCertStore.Open('MY');  // 个人证书

  // 枚举证书
  for Cert in Store.Certificates do
    WriteLn(Cert.Subject);

  // 按主题查找
  Cert := Store.FindBySubject('CN=MyClient');
end.
```

## 优势

1. **零依赖**: 无需安装额外 DLL
2. **系统集成**: 自动使用 Windows 证书存储
3. **自动更新**: 通过 Windows Update 获取安全更新
4. **平台生态集成**: 与 Schannel / 系统证书存储 / 组策略配置保持一致
5. **企业功能**: 支持组策略配置

## 限制与注意事项

1. **仅 Windows**: 不支持其他操作系统
2. **PSK 不支持**: Schannel 不支持预共享密钥
3. **Ed25519 不支持**: 不支持 Edwards 曲线
4. **API 限制**: 某些高级功能需要特定 Windows 版本
5. **调试困难**: 错误信息不如 OpenSSL 详细
6. **Capability truth**: `IsCipherSupported(...)` 现在只对 capability-matrix 中的已知 cipher family 返回 `True`；未知/fake cipher name 会被拒绝，而不是再以“系统握手期再决定”为由放行

## 故障排除

### 常见错误

| 错误码                   | 含义           | 解决方案                |
| ------------------------ | -------------- | ----------------------- |
| SEC_E_ALGORITHM_MISMATCH | 算法不匹配     | 检查 TLS 版本和密码套件 |
| SEC_E_CERT_EXPIRED       | 证书过期       | 更新证书                |
| SEC_E_UNTRUSTED_ROOT     | 不信任的根证书 | 安装根证书到系统存储    |
| SEC_E_WRONG_PRINCIPAL    | 主机名不匹配   | 检查 SNI 设置           |

### 调试技巧

```pascal
// 启用详细日志
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslWinSSL)
  .WithInfoCallback(MyLogCallback)
  .BuildClient;

procedure MyLogCallback(const Info: TSSLInfoRecord);
begin
  WriteLn('[WinSSL] ', Info.Message);
end;
```

## 相关文档

- `src/fafafa.ssl.winssl.*.pas` - WinSSL 后端实现
- `examples/winssl_*.pas` - WinSSL 示例
- `docs/guides/QUICKSTART.md` - WinSSL Session 复用示例
