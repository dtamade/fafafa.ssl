# fafafa.ssl API 文档

**版本:** rolling
**更新日期:** 2026-05-19

---

## 目录

1. [快速入门](#快速入门)
2. [核心 API](#核心-api)
3. [OCSP Stapling](#ocsp-stapling)
4. [证书透明度 (CT)](#证书透明度-ct)
5. [会话缓存](#会话缓存)
6. [性能优化](#性能优化)
7. [最佳实践](#最佳实践)
8. [故障排查](#故障排查)

---

## 快速入门

### 5 分钟上手

下面这段 `5 分钟上手` 展示的是 active API reference 的 direct `ISSLConnection` / owner-surface reference，不是普通新代码唯一推荐的 TLS bootstrap 入口。
如果你只是普通客户端/服务端接入，优先回到 `docs/guides/GETTING_STARTED.md` 里的 `TSSLConnector` / `TSSLAcceptor` / `TSSLStream` 主路径。
这里之所以仍直接展示 `CreateConnection(...)`，是因为本页后续还会继续展开挂在连接对象上的 owner surface，例如 `ISSLOCSPStapling` / `ISSLCertificateVerification`。

```pascal
program QuickStart;

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder;

var
  Builder: ISSLContextBuilder;
  Context: ISSLContext;
  Connection: ISSLConnection;
  ClientConn: ISSLClientConnection;
  Socket: THandle;
  Response: string;
begin
  // 1. 创建 SSL 上下文
  Builder := TSSLContextBuilder.Create;
  Builder
    .WithTLS12And13                    // 使用 TLS 1.2 和 1.3
    .WithVerifyPeer                    // 验证对端证书
    .WithOCSPStapling(True)            // 启用 OCSP Stapling
    .WithSystemRoots;                  // 使用系统根证书

  Context := Builder.BuildClient;

  // 2. 先由 transport 层建立 TCP 连接
  Socket := ConnectToServer('example.com', 443);
  Connection := Context.CreateConnection(Socket);
  if Supports(Connection, ISSLClientConnection, ClientConn) then
    ClientConn.SetServerName('example.com');

  // 3. 连接到服务器
  if Connection.Connect then
  begin
    WriteLn('连接成功!');
    WriteLn('协议版本: ', Connection.GetProtocolVersion);
    WriteLn('密码套件: ', Connection.GetCipherName);

    // 4. 发送和接收数据
    Connection.WriteString('GET / HTTP/1.1'#13#10 +
      'Host: example.com'#13#10#13#10);
    if Connection.ReadString(Response) then
      WriteLn(Response);

    Connection.Shutdown;
  end;
end.
```

---

## 核心 API

### ISSLContextBuilder

SSL 上下文构建器,使用流式 API 配置 SSL/TLS 参数。

#### 方法

##### WithTLS12And13

```pascal
function WithTLS12And13: ISSLContextBuilder;
```

启用 TLS 1.2 和 TLS 1.3 协议。

**示例:**

```pascal
Builder.WithTLS12And13;
```

##### WithVerifyPeer

```pascal
function WithVerifyPeer: ISSLContextBuilder;
```

启用对端证书验证。

**示例:**

```pascal
Builder.WithVerifyPeer;
```

##### WithCertificate

```pascal
function WithCertificate(const ACertFile: string): ISSLContextBuilder;
```

加载服务器证书。

**参数:**

- `ACertFile`: 证书文件路径 (PEM 格式)

**示例:**

```pascal
Builder.WithCertificate('server.crt');
```

##### WithPrivateKey

```pascal
function WithPrivateKey(const AKeyFile: string): ISSLContextBuilder;
```

加载私钥。

**参数:**

- `AKeyFile`: 私钥文件路径 (PEM 格式)

**示例:**

```pascal
Builder.WithPrivateKey('server.key');
```

##### WithOCSPStapling

```pascal
function WithOCSPStapling(AEnabled: Boolean = True): ISSLContextBuilder;
```

启用 OCSP stapling 相关的 context 选项。

**参数:**

- `AEnabled`: 是否启用 (默认 True)

**说明:**

- 在当前有 fresh evidence 的 client/runtime path 上，这会请求和消费 stapled OCSP response。
- 不要把它理解成“所有 backend/server path 都会自动获取并附加 OCSP 响应”。

**示例:**

```pascal
Builder.WithOCSPStapling(True);
```

##### WithOCSPStaplingRequired

```pascal
function WithOCSPStaplingRequired(ARequired: Boolean = True): ISSLContextBuilder;
```

设置是否强制要求 OCSP Stapling。

**参数:**

- `ARequired`: 是否强制要求 (默认 True)

**说明:**

- 在当前支持的 client/runtime path 上，`verify-peer` 的 non-resumed full-handshake 才会对缺失或未通过当前有界校验的 stapled response fail-closed。
- `verify-none` 和 resumed TLS 1.3 path 不会触发这条 required enforcement。
- 这仍然属于 stapled-response path，不等于完整在线 revocation parity。

**示例:**

```pascal
Builder
  .WithOCSPStapling(True)
  .WithOCSPStaplingRequired(True);  // 强制要求 Stapling
```

##### WithServerOCSPStapledResponseFile

```pascal
function WithServerOCSPStapledResponseFile(const AFile: string): ISSLContextBuilder;
```

为服务端 context 配置 caller-provided stapled OCSP response DER 文件。

**参数:**

- `AFile`: stapled OCSP response DER 文件路径

**说明:**

- 该配置在 `BuildServer` 时应用。
- 如果 backend 实现了 `ISSLServerOCSPStaplingContext`，builder 会在构建阶段调用 `LoadServerStapledOCSPResponseFile(AFile)`。
- 如果配置了该字段但 backend 不支持 `ISSLServerOCSPStaplingContext`，`BuildServer` 会直接抛出清晰配置错误，不会 silent ignore。
- 这只负责 caller-provided material，不负责 online fetch、refresh，或 responder 调度。

**示例:**

```pascal
Builder.WithServerOCSPStapledResponseFile('fixtures/ocsp/server_leaf.ocsp.der');
```

##### WithCertificateTransparencyRequired

```pascal
function WithCertificateTransparencyRequired(ARequired: Boolean = True): ISSLContextBuilder;
```

设置是否在当前 FreePascal client/runtime path 上强制要求 Certificate Transparency。

**参数:**

- `ARequired`: 是否强制要求 (默认 True)

**说明:**

- 在当前支持的 client/runtime path 上，`verify-peer` 的 non-resumed full-handshake 才会执行这条 required gate。
- `verify-none` 和 resumed TLS 1.3 path 不会触发这条 required enforcement。
- 一旦 gate 生效，missing SCT、validation unavailable、policy failed 都会 fail-closed。
- 这只描述当前连接级 runtime surface，不等于所有 backend 都已经具备一致的 CT enforcement。

**示例:**

```pascal
Builder.WithCertificateTransparencyRequired(True);
```

##### BuildClient / BuildServer

```pascal
function BuildClient: ISSLContext;
function BuildServer: ISSLContext;
```

构建客户端或服务端 SSL 上下文。

**示例:**

```pascal
ClientContext := Builder.BuildClient;
ServerContext := Builder.BuildServer;
```

---

### ISSLConnection

SSL/TLS 连接接口。

下面列的是当前常用连接方法切片，不是 `v1.5.0` 当前 shipped source 的完整逐行镜像。
完整 source-truth 请看 `docs/reference/API_REFERENCE.md`。

#### 方法

##### Connect

```pascal
function Connect: Boolean;
```

对已经绑定到 socket/stream transport 的连接执行 TLS 握手。

**返回值:**

- `True`: 连接成功
- `False`: 连接失败

**示例:**

```pascal
if Connection.Connect then
  WriteLn('连接成功');
```

##### Write

```pascal
function Write(const ABuffer; ACount: Integer): Integer;
```

发送原始字节数据。

**参数:**

- `ABuffer`: 数据缓冲区
- `ACount`: 要发送的字节数

**返回值:**

- 实际发送的字节数

**示例:**

```pascal
BytesSent := Connection.Write(Request[1], Length(Request));
```

##### Read

```pascal
function Read(var ABuffer; ACount: Integer): Integer;
```

接收原始字节数据。

**参数:**

- `ABuffer`: 接收缓冲区
- `ACount`: 最大接收字节数

**返回值:**

- 实际接收的字节数

**示例:**

```pascal
BytesRead := Connection.Read(Buffer, SizeOf(Buffer));
```

##### WriteString

```pascal
function WriteString(const AStr: string): Boolean;
```

发送文本数据。

**示例:**

```pascal
if not Connection.WriteString('Hello, World!') then
  WriteLn('发送失败');
```

##### ReadString

```pascal
function ReadString(out AStr: string): Boolean;
```

读取文本数据。

**示例:**

```pascal
if Connection.ReadString(Response) then
  WriteLn(Response);
```

下面这组 `GetOCSP*` 条目之所以仍保留在 `ISSLConnection` 小节，是因为当前 shipped source 仍向后兼容这些 compatibility-core mirrors。
新代码优先通过 `ISSLOCSPStapling` 读取 stapling 状态 / response / verify status / status string。

##### GetOCSPStaplingEnabled

```pascal
function GetOCSPStaplingEnabled: Boolean;
```

检查是否启用了 OCSP Stapling。

**返回值:**

- `True`: 已启用
- `False`: 未启用

**示例:**

```pascal
var
  OCSP: ISSLOCSPStapling;
begin
  if Supports(Connection, ISSLOCSPStapling, OCSP) and OCSP.GetOCSPStaplingEnabled then
    WriteLn('OCSP Stapling 已启用');
end;
```

##### GetOCSPResponse

```pascal
function GetOCSPResponse: TBytes;
```

获取 OCSP 响应 (DER 编码)。

**返回值:**

- OCSP 响应的字节数组,未提供时返回空数组

**示例:**

```pascal
var
  OCSP: ISSLOCSPStapling;
  OCSPResponse: TBytes;
begin
  if Supports(Connection, ISSLOCSPStapling, OCSP) then
  begin
    OCSPResponse := OCSP.GetOCSPResponse;
    if Length(OCSPResponse) > 0 then
      WriteLn('收到 OCSP 响应: ', Length(OCSPResponse), ' 字节');
  end;
end;
```

##### IsOCSPResponseVerified

```pascal
function IsOCSPResponseVerified: Boolean;
```

检查 OCSP 响应是否已验证。

**返回值:**

- `True`: 已验证且证书状态为 Good
- `False`: 未验证或验证失败

**示例:**

```pascal
var
  OCSP: ISSLOCSPStapling;
begin
  if Supports(Connection, ISSLOCSPStapling, OCSP) and OCSP.IsOCSPResponseVerified then
    WriteLn('OCSP 响应已验证,证书有效');
end;
```

##### GetOCSPResponseStatus

```pascal
function GetOCSPResponseStatus: string;
```

获取 OCSP 响应状态描述。

**返回值:**

- 状态描述字符串 (如 "Good", "Revoked", "Unknown", "Not Provided")

**示例:**

```pascal
var
  OCSP: ISSLOCSPStapling;
begin
  if Supports(Connection, ISSLOCSPStapling, OCSP) then
    WriteLn('OCSP 状态: ', OCSP.GetOCSPResponseStatus);
end;
```

---

## OCSP Stapling

### 概述

OCSP Stapling 允许服务器在 TLS 握手期间提供 OCSP 响应,客户端无需单独查询 OCSP 服务器,提升性能和隐私。

当前最可验证、最直接可用的路径是 client-side stapled-response request/consume。
如果你要主动构造和发送 OCSP 请求，请改用 OpenSSL OCSP API 工作流，而不要把本节当成完整在线 revocation 文档。

### 客户端配置

```pascal
Builder := TSSLContextBuilder.Create;
Builder
  .WithTLS12And13
  .WithVerifyPeer
  .WithOCSPStapling(True)              // 启用 OCSP Stapling
  .WithOCSPStaplingRequired(False);    // 不强制要求 (可选)

Context := Builder.BuildClient;
```

在当前 client/runtime path 上：

- `WithOCSPStapling(True)` 会请求 stapled response。
- `WithOCSPStaplingRequired(True)` 会在 `verify-peer` 的 non-resumed full-handshake path 上，对缺失或未通过当前有界校验的 stapled response fail-closed。
- 如果关闭 `verify-peer`，当前实现不会因为 `required` 被 fail-closed。
- 对 resumed TLS 1.3 path，`required` 也不会因为 resumed flight 缺少新的 stapled response 被触发。

### 服务端配置

当前 public server-side stapling path 是 bounded 的 caller-provided material 配置。
不要把下面的配置理解成“库会自动在线获取并附加 OCSP 响应”。

```pascal
Builder := TSSLContextBuilder.Create;
Builder
  .WithCertificate('server.crt')
  .WithPrivateKey('server.key')
  .WithOCSPStapling(True)
  .WithServerOCSPStapledResponseFile('fixtures/ocsp/server_leaf.ocsp.der');

Context := Builder.BuildServer;
```

如果这个 server context 只是普通单向 TLS，请在 builder 上显式加 `.WithVerifyNone`；如果要做 mTLS，请改成 `.WithMutualTLS(...)`。

在当前 server/runtime path 上：

- `WithServerOCSPStapledResponseFile(...)` 会在 `BuildServer` 时把 DER 文件加载到 `ISSLServerOCSPStaplingContext`。
- 只有 `full handshake + client requested status_request + context configured stapled response` 三个条件同时满足时，服务端才会发出 stapled response。
- 如果 backend 不支持 `ISSLServerOCSPStaplingContext`，但 builder 配置了 `server_ocsp_stapled_response_file`，构建会直接报错。

### 服务端 stapling 上下文访问

`ISSLServerOCSPStaplingContext` 是一个可选 public interface，用于访问服务端 stapled OCSP response material：

```pascal
procedure ClearServerStapledOCSPResponse;
procedure SetServerStapledOCSPResponse(const AResponseDER: TBytes);
procedure LoadServerStapledOCSPResponseFile(const AFileName: string);
function HasServerStapledOCSPResponse: Boolean;
function GetServerStapledOCSPResponse: TBytes;
```

示例：

```pascal
var
  ServerStapling: ISSLServerOCSPStaplingContext;
begin
  if Supports(Context, ISSLServerOCSPStaplingContext, ServerStapling) then
    WriteLn(Length(ServerStapling.GetServerStapledOCSPResponse));
end;
```

### 检查 OCSP 状态

```pascal
Connection := Context.CreateConnection(Socket);
if Supports(Connection, ISSLClientConnection, ClientConn) then
  ClientConn.SetServerName('example.com');
if Connection.Connect then
begin
  if Supports(Connection, ISSLOCSPStapling, OCSP) then
  begin
    if OCSP.GetOCSPStaplingEnabled then
      WriteLn('OCSP Stapling 已启用');

    OCSPResponse := OCSP.GetOCSPResponse;
    if Length(OCSPResponse) > 0 then
      WriteLn('收到 OCSP 响应: ', Length(OCSPResponse), ' 字节');

    if OCSP.IsOCSPResponseVerified then
      WriteLn('证书有效')
    else
      WriteLn('证书验证失败或未验证');

    WriteLn('OCSP 状态: ', OCSP.GetOCSPResponseStatus);
  end;
end;
```

如果你在写新代码，优先通过 `ISSLOCSPStapling.GetOCSPStaplingEnabled` 判断当前连接是否启用了 stapling 读取面。
需要读取 stapled response bytes 时，优先通过 `ISSLOCSPStapling.GetOCSPResponse`。
需要检查当前 stapled response 是否已经过当前实现的验证路径时，优先通过 `ISSLOCSPStapling.IsOCSPResponseVerified`。
需要读取状态描述字符串时，优先通过 `ISSLOCSPStapling.GetOCSPResponseStatus`。
如果你已经直接依赖 `Connection.GetOCSP*` 方法，也仍然可以继续把它们当作 compatibility-core mirrors 使用。

### 性能指标

**OCSP Stapling 缓存性能:**

- Put 吞吐量: 86,207 ops/sec
- Get 吞吐量: 134,228 ops/sec
- 并发吞吐量 (4线程): 597,015 ops/sec
- 平均延迟: 11 μs
- 内存效率: 0.30 KB/entry

---

## 证书透明度 (CT)

### 概述

证书透明度 (Certificate Transparency) 是一种安全机制,通过公开日志记录所有 SSL/TLS 证书,防止恶意证书颁发。

当前最直接、最可验证的路径是 FreePascal client/runtime CT surface。
如果你只是想知道服务端有没有给出 SCT、默认 validation/policy 结果是什么，或者想把 CT 作为连接级 fail-closed 条件，先用这一条路径。
只有在你需要更底层、更自定义的 CT 校验流程时，再直接使用 `TSCTValidator`。

### 客户端 runtime 配置

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  CertVerify: ISSLCertificateVerification;
  CT: ISSLCertificateTransparency;
  CTValidation: ISSLCertificateTransparencyValidation;
begin
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslFreePascal)
    .WithTLS13
    .WithVerifyPeer
    .WithSystemRoots
    .WithCertificateTransparencyRequired(True)
    .BuildClient;

  Conn := Ctx.CreateConnection(Socket);
  (Conn as ISSLClientConnection).SetServerName('example.com');

  if not Conn.Connect then
  begin
    if Supports(Conn, ISSLCertificateVerification, CertVerify) then
      raise Exception.Create(CertVerify.GetVerifyResultString)
    else
      raise Exception.Create('TLS handshake failed');
  end;

  if Supports(Conn, ISSLCertificateTransparency, CT) then
  begin
    WriteLn('CT enabled: ', CT.GetCertificateTransparencyEnabled);
    WriteLn('SCT count: ', CT.GetSignedCertificateTimestampCount);
    WriteLn('CT status: ', CT.GetCertificateTransparencyStatus);
  end;

  if Supports(Conn, ISSLCertificateTransparencyValidation, CTValidation) then
  begin
    WriteLn('CT validation: ', CTValidation.GetCertificateTransparencyValidationStatus);
    WriteLn('CT policy satisfied: ', CTValidation.IsCertificateTransparencyPolicySatisfied);
  end;
end;
```

在当前 runtime path 上：

- `ISSLCertificateTransparency` 暴露 SCT enabled/count/status 等 surface。
- `ISSLCertificateTransparencyValidation` 暴露 validation result、policy satisfied 与状态文本。
- `WithCertificateTransparencyRequired(True)` 只会在 `verify-peer` 的 non-resumed full-handshake path 上执行 fail-closed。
- 如果关闭 `verify-peer`，当前实现不会因为 `required` 被 fail-closed。
- 对 resumed TLS 1.3 path，`required` 也不会因为 resumed flight 没有新的 certificate/SCT material 被触发。

当前 gate 生效时，只有三类 fail-closed 条件：

- 服务端没有提供 SCT list
- CT validation 结果不可用
- 默认 CT policy 不满足

当前 FreePascal client 会优先 surface TLS `signed_certificate_timestamp` 扩展；如果这个扩展缺失，则回退到 leaf X.509 里的 embedded SCT 扩展。

当前不要把这条路径理解成“所有 CT source/backend 都已完成”。还没有写成已支持的范围包括：

- OCSP-delivered SCT source
- 自定义 CT policy 的连接级 enforcement
- 所有 backend 的一致支持声明

### 需要底层 CT validator 时，再用 `TSCTValidator`

```pascal
uses
  fafafa.ssl.ct.sct;

var
  Validator: TSCTValidator;
  Result: TSCTValidationResult;
begin
  // 创建 SCT 验证器
  Validator := TSCTValidator.Create;
  try
    // 配置验证策略
    Validator.SetPolicy(sctpRequireAtLeast2SCTs);

    // 从证书验证 SCT
    Result := Validator.ValidateFromCertificate(Certificate);

    if Result.IsValid then
      WriteLn('CT 验证通过')
    else
      WriteLn('CT 验证失败: ', Result.ErrorMessage);
  finally
    Validator.Free;
  end;
end;
```

### CT 验证策略

```pascal
type
  TSCTPolicy = (
    sctpNoSCTRequired,           // 不要求 SCT
    sctpRequireAtLeast1SCT,      // 至少 1 个 SCT
    sctpRequireAtLeast2SCTs,     // 至少 2 个 SCT (推荐)
    sctpRequireAtLeast3SCTs      // 至少 3 个 SCT (严格)
  );
```

---

## 会话缓存

### 概述

会话缓存允许调用方保存并重新注入 TLS 会话候选，但是否真的避免了完整握手，仍取决于 backend、目标站点、上下文复用和 native observed-reuse truth。

### 使用会话缓存

```pascal
uses
  fafafa.ssl.session.cache;

var
  SessionCache: TSSLSessionCache;
  SessionResumption: ISSLSessionResumption;
  Session: ISSLSession;
begin
  // 创建会话缓存
  SessionCache := TSSLSessionCache.Create(1000);  // 最多 1000 个会话
  try
    // 保存会话
    if Supports(Connection, ISSLSessionResumption, SessionResumption) then
    begin
      Session := SessionResumption.GetSession;
      SessionCache.Put('example.com', 443, Session);
    end;

    // 复用会话
    Session := SessionCache.Get('example.com', 443);
    if (Session <> nil) and Supports(Connection, ISSLSessionResumption, SessionResumption) then
    begin
      SessionResumption.SetSession(Session);
      WriteLn('已为下一次握手配置会话候选');
    end;

    // 获取统计信息
    Stats := SessionCache.GetStats;
    WriteLn('会话数: ', Stats.TotalSessions);
    WriteLn('命中率: ', Stats.HitRate:0:2, '%');
    WriteLn('复用率: ', Stats.ReuseRate:0:2, '%');
  finally
    SessionCache.Free;
  end;
end;
```

> 对 `MbedTLS` 而言，当前 public surface 已发布 session save/load 与 `SetSession(...)` candidate path，但 local source/header truth 只有 `mbedtls_ssl_set_session` / `mbedtls_ssl_get_session` / `mbedtls_ssl_session_load/save`；没有 public reused getter。
> 因而这段示例在 MbedTLS 上表达的是“已配置候选 session”，不等于已经观测到 resumed handshake。

### 会话持久化

```pascal
// 保存会话到文件
if SessionCache.SaveToFile('sessions.dat') then
  WriteLn('会话已保存');

// 从文件加载会话
if SessionCache.LoadFromFile('sessions.dat') then
  WriteLn('会话已加载');
```

### 性能指标

**会话缓存性能:**

- Put 吞吐量: 86,207 ops/sec
- Get 吞吐量: 1,300,000 ops/sec
- 平均延迟: < 0.1 ms
- 内存效率: < 2 KB/session

---

## 性能优化

### OCSP Stapling 优化

**延迟清理机制:**

```pascal
// 默认配置已优化,每 100 次 Put 清理一次
Cache := TOCSPResponseCache.Create(10000);
```

**分片锁架构:**

- 16 个独立分片
- 并发吞吐量: 597K ops/sec (4线程)
- 接近线性扩展

### 会话缓存优化

**O(1) 哈希表查找:**

```pascal
// 默认使用哈希表,查找延迟 < 0.1ms
SessionCache := TSSLSessionCache.Create(1000);
```

**自动过期清理:**

```pascal
// 默认超时 5 分钟
SessionCache := TSSLSessionCache.Create(1000, 300);  // 300 秒
```

---

## 最佳实践

### 1. 客户端配置

```pascal
Builder := TSSLContextBuilder.Create;
Builder
  .WithTLS12And13                    // 使用现代协议
  .WithVerifyPeer                    // 验证证书
  .WithOCSPStapling(True)            // 启用 OCSP Stapling
  .WithOCSPStaplingRequired(False)   // 高风险路径可改成 True
  .WithSystemRoots                   // 使用系统根证书
  .WithOption(ssoNoSSLv2)            // 禁用旧协议
  .WithOption(ssoNoSSLv3);

Context := Builder.BuildClient;
```

### 2. 服务端配置

```pascal
Builder := TSSLContextBuilder.Create;
Builder
  .WithBackend(sslOpenSSL)
  .WithCertificate('server.crt')
  .WithPrivateKey('server.key')
  .WithTLS12And13
  .WithSafeDefaults
  .WithOCSPStapling(True)
  .WithServerOCSPStapledResponseFile('fixtures/ocsp/server_leaf.ocsp.der');

Context := Builder.BuildServer;
```

如果这个 server context 只是普通单向 TLS，请在 builder 上显式加 `.WithVerifyNone`；如果要做 mTLS，请改成 `.WithMutualTLS(...)`。
如需 custom cipher allowlist，请只在 `SupportsCustomCipherSuites=True` 的 backend（当前主要是 OpenSSL）上追加这类 builder 配置。

### 3. 错误处理

```pascal
var
  CertVerify: ISSLCertificateVerification;
begin
try
  if Connection.Connect then
  begin
    // 处理连接
  end
  else
  begin
    if Supports(Connection, ISSLCertificateVerification, CertVerify) and
       (CertVerify.GetVerifyResult <> 0) then
      WriteLn('证书验证失败: ', CertVerify.GetVerifyResultString)
    else
      WriteLn('TLS 握手失败');
  end;
except
  on E: ESSLException do
    WriteLn('SSL 错误: ', E.Message);
end;
end;
```

### 4. 资源管理

```pascal
// 使用接口自动管理资源
var
  Context: ISSLContext;
  Connection: ISSLConnection;
begin
  Context := TSSLContextBuilder.Create.BuildClient;
  Connection := Context.CreateConnection(MySocket);

  // 使用连接...

  // 接口自动释放,无需手动 Free
end;
```

---

## 故障排查

### 常见问题

#### 1. 连接失败

**问题:** `Connection.Connect` 返回 False

**解决方案:**

```pascal
var
  CertVerify: ISSLCertificateVerification;
begin
  // 当前连接失败时，优先检查证书验证结果
  if Supports(Connection, ISSLCertificateVerification, CertVerify) and
     (CertVerify.GetVerifyResult <> 0) then
    WriteLn('证书验证失败: ', CertVerify.GetVerifyResultString);

  // 对 client 连接，确认是否已在 Connect 前设置 per-connection SNI
  // (Connection as ISSLClientConnection).SetServerName('example.com');
end;
```

#### 2. OCSP Stapling 未工作

**问题:** `GetOCSPResponse` 返回空数组

**解决方案:**

```pascal
if Supports(Connection, ISSLOCSPStapling, OCSP) then
begin
  if not OCSP.GetOCSPStaplingEnabled then
    WriteLn('OCSP Stapling 未启用');

  WriteLn('OCSP 状态: ', OCSP.GetOCSPResponseStatus);
end;
```

另外确认：

- client path 已启用 `WithVerifyPeer` + `WithOCSPStapling(True)`
- 如果你开了 `WithOCSPStaplingRequired(True)`，在 `verify-peer` 的 non-resumed full-handshake path 上，缺失或未通过当前有界校验的 response 会直接让握手失败
- 不要默认假设服务端一定会提供 stapled response

#### 3. 性能问题

**问题:** 连接速度慢

**解决方案:**

```pascal
// 启用会话复用
SessionCache := TSSLSessionCache.Create(1000);
Session := SessionCache.Get(Host, Port);
if (Session <> nil) and Supports(Connection, ISSLSessionResumption, SessionResumption) then
  SessionResumption.SetSession(Session);

// 启用 OCSP Stapling
Builder.WithOCSPStapling(True);
```

#### 4. 内存使用高

**问题:** 内存占用过高

**解决方案:**

```pascal
// 限制缓存大小
SessionCache := TSSLSessionCache.Create(100);  // 减少到 100

// 定期清理
SessionCache.Clear;
```

---

## 更多资源

- [GitHub 仓库](https://github.com/fafafa/fafafa.ssl)
- [OCSP 模块测试报告](../test_reports/P2_OCSP_MODULE_REPORT.md)
- [CT 实现指南](../guides/CT_IMPLEMENTATION_GUIDE.md)
- [示例代码](../../examples/)

---

**版权所有 © 2026 fafafa.ssl team**
