# fafafa.ssl API 文档

**版本:** 2.0.0  
**更新日期:** 2026-01-31

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

```pascal
program QuickStart;

uses
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.context.builder;

var
  Builder: ISSLContextBuilder;
  Context: ISSLContext;
  Connection: ISSLConnection;
begin
  // 1. 创建 SSL 上下文
  Builder := TSSLContextBuilder.Create;
  Builder
    .WithTLS12And13                    // 使用 TLS 1.2 和 1.3
    .WithVerifyPeer                    // 验证对端证书
    .WithOCSPStapling(True)            // 启用 OCSP Stapling
    .WithSystemRootCerts;              // 使用系统根证书
  
  Context := Builder.BuildClient;
  
  // 2. 创建连接
  Connection := Context.CreateConnection(443);
  
  // 3. 连接到服务器
  if Connection.Connect('example.com', 443) then
  begin
    WriteLn('连接成功!');
    WriteLn('协议版本: ', Connection.GetProtocolVersion);
    WriteLn('密码套件: ', Connection.GetCipherName);
    
    // 4. 发送和接收数据
    Connection.Write('GET / HTTP/1.1'#13#10'Host: example.com'#13#10#13#10);
    // ... 读取响应
    
    Connection.Disconnect;
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
启用 OCSP Stapling。

**参数:**
- `AEnabled`: 是否启用 (默认 True)

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

**示例:**
```pascal
Builder
  .WithOCSPStapling(True)
  .WithOCSPStaplingRequired(True);  // 强制要求 Stapling
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

#### 方法

##### Connect
```pascal
function Connect(const AHost: string; APort: Word): Boolean;
```
连接到服务器。

**参数:**
- `AHost`: 主机名或 IP 地址
- `APort`: 端口号

**返回值:**
- `True`: 连接成功
- `False`: 连接失败

**示例:**
```pascal
if Connection.Connect('example.com', 443) then
  WriteLn('连接成功');
```

##### Write
```pascal
function Write(const AData: TBytes): Integer;
function Write(const AData: string): Integer;
```
发送数据。

**参数:**
- `AData`: 要发送的数据

**返回值:**
- 实际发送的字节数

**示例:**
```pascal
BytesSent := Connection.Write('Hello, World!');
```

##### Read
```pascal
function Read(var ABuffer: TBytes; AMaxLen: Integer): Integer;
```
接收数据。

**参数:**
- `ABuffer`: 接收缓冲区
- `AMaxLen`: 最大接收字节数

**返回值:**
- 实际接收的字节数

**示例:**
```pascal
SetLength(Buffer, 4096);
BytesRead := Connection.Read(Buffer, 4096);
```

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
if Connection.GetOCSPStaplingEnabled then
  WriteLn('OCSP Stapling 已启用');
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
OCSPResponse := Connection.GetOCSPResponse;
if Length(OCSPResponse) > 0 then
  WriteLn('收到 OCSP 响应: ', Length(OCSPResponse), ' 字节');
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
if Connection.IsOCSPResponseVerified then
  WriteLn('OCSP 响应已验证,证书有效');
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
WriteLn('OCSP 状态: ', Connection.GetOCSPResponseStatus);
```

---

## OCSP Stapling

### 概述

OCSP Stapling 允许服务器在 TLS 握手期间提供 OCSP 响应,客户端无需单独查询 OCSP 服务器,提升性能和隐私。

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

### 服务端配置

```pascal
Builder := TSSLContextBuilder.Create;
Builder
  .WithCertificate('server.crt')
  .WithPrivateKey('server.key')
  .WithOCSPStapling(True);             // 服务端自动获取和附加响应

Context := Builder.BuildServer;
```

### 检查 OCSP 状态

```pascal
Connection := Context.CreateConnection(443);
if Connection.Connect('example.com', 443) then
begin
  // 检查是否启用
  if Connection.GetOCSPStaplingEnabled then
    WriteLn('OCSP Stapling 已启用');
  
  // 获取响应
  OCSPResponse := Connection.GetOCSPResponse;
  if Length(OCSPResponse) > 0 then
    WriteLn('收到 OCSP 响应: ', Length(OCSPResponse), ' 字节');
  
  // 检查验证状态
  if Connection.IsOCSPResponseVerified then
    WriteLn('证书有效')
  else
    WriteLn('证书验证失败');
  
  // 获取状态描述
  WriteLn('OCSP 状态: ', Connection.GetOCSPResponseStatus);
end;
```

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

### 启用 CT 验证

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

会话缓存允许 TLS 会话复用,避免完整握手,显著提升性能。

### 使用会话缓存

```pascal
uses
  fafafa.ssl.session.cache;

var
  SessionCache: TSSLSessionCache;
  Session: ISSLSession;
begin
  // 创建会话缓存
  SessionCache := TSSLSessionCache.Create(1000);  // 最多 1000 个会话
  try
    // 保存会话
    Session := Connection.GetSession;
    SessionCache.Put('example.com', 443, Session);
    
    // 复用会话
    Session := SessionCache.Get('example.com', 443);
    if Session <> nil then
    begin
      Connection.SetSession(Session);
      WriteLn('会话复用成功');
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
  .WithSystemRootCerts               // 使用系统根证书
  .WithOption(ssoNoSSLv2)            // 禁用旧协议
  .WithOption(ssoNoSSLv3);

Context := Builder.BuildClient;
```

### 2. 服务端配置

```pascal
Builder := TSSLContextBuilder.Create;
Builder
  .WithCertificate('server.crt')
  .WithPrivateKey('server.key')
  .WithTLS12And13
  .WithOCSPStapling(True)            // 自动获取 OCSP 响应
  .WithCipherList('HIGH:!aNULL:!MD5'); // 强密码套件

Context := Builder.BuildServer;
```

### 3. 错误处理

```pascal
try
  if Connection.Connect('example.com', 443) then
  begin
    // 处理连接
  end
  else
  begin
    WriteLn('连接失败: ', Connection.GetLastError);
  end;
except
  on E: ESSLException do
    WriteLn('SSL 错误: ', E.Message);
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
  Connection := Context.CreateConnection(443);
  
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
// 检查错误信息
WriteLn('错误: ', Connection.GetLastError);

// 检查证书验证
if not Connection.GetPeerCertificateVerified then
  WriteLn('证书验证失败');
```

#### 2. OCSP Stapling 未工作

**问题:** `GetOCSPResponse` 返回空数组

**解决方案:**
```pascal
// 检查是否启用
if not Connection.GetOCSPStaplingEnabled then
  WriteLn('OCSP Stapling 未启用');

// 检查服务器支持
WriteLn('OCSP 状态: ', Connection.GetOCSPResponseStatus);
```

#### 3. 性能问题

**问题:** 连接速度慢

**解决方案:**
```pascal
// 启用会话复用
SessionCache := TSSLSessionCache.Create(1000);
Session := SessionCache.Get(Host, Port);
if Session <> nil then
  Connection.SetSession(Session);

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
- [性能测试报告](docs/OCSP_PERFORMANCE_REPORT.md)
- [CT 实现指南](docs/CT_IMPLEMENTATION_GUIDE.md)
- [示例代码](examples/)

---

**版权所有 © 2026 fafafa.ssl team**
