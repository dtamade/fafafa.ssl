# TLS 1.3 Early Data 使用指南

## 概述

TLS 1.3 Early Data（0-RTT）允许客户端在 TLS 握手完成前发送应用数据，显著降低连接延迟。

**性能提升**：

- 首次连接：无改善（需要完整握手）
- 恢复连接：延迟降低 1 RTT（约 50-100ms）

**安全注意**：

- Early Data 可能被重放攻击
- 仅用于幂等操作（GET 请求等）
- 不要用于状态改变操作（POST、PUT、DELETE）

---

## 后端支持情况

| 后端       | 客户端 Early Data    | 服务端 Early Data    | 状态                       |
| ---------- | -------------------- | -------------------- | -------------------------- |
| FreePascal | ✅ 已接通（实验性）  | ✅ 已接通（实验性）  | 实验性                     |
| OpenSSL    | ✅ 完整支持          | ✅ 完整支持          | 生产就绪 (v1.4.1+)         |
| WinSSL     | ❌ 不支持            | ❌ 不支持            | Schannel API 限制          |
| MbedTLS    | ❌ 不支持            | ❌ 不支持            | 当前不暴露 early-data 接口 |
| WolfSSL    | ⚠️ helper 完整时可用 | ⚠️ helper 完整时可用 | 按构建/运行时 helper 门控  |

在继续之前先认清当前边界：

- 生产路径默认按 `OpenSSL` 理解。
- `FreePascal` 的 client/server surface 已接通，但能力仍按 experimental 发布；默认 replay truth 落到本地持久化 replay-store，默认路径不可用或不可写时 fail-closed reject。
- 若当前 `wolfSSL` 动态库未导出 early-data helpers：`wolfSSL_write_early_data`、`wolfSSL_get_early_data_status`、`wolfSSL_CTX_set_max_early_data`、`wolfSSL_CTX_get_max_early_data`，则 capability 会退化为 `none`，context / connection 都不会暴露 early-data 接口。
- `WinSSL` / `MbedTLS` 当前不支持 early-data，因此示例里的 `Supports(...)` 检查必须保留。

---

## 快速开始

本页代码示例默认使用 `OpenSSL`，因为这是当前唯一可直接按 production-ready 看待的 early-data 路径。切到 `FreePascal` 或 `WolfSSL` 前，先检查 capability 和可选接口是否真实存在。
这里直接回到 `CreateConnection(...)`，是因为 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` 这组 early-data owner surface 分别挂在 context / connection 对象上；如果你只是普通客户端接入而不需要 early-data owner surface，握手入口仍可保持在 `TSSLConnector` / `TSSLStream`。

### 客户端 Early Data

```pascal
uses fafafa.ssl;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  EarlyDataCtx: ISSLEarlyDataContext;
  EarlyDataConn: ISSLEarlyDataConnection;
  Request: TBytes;
begin
  // 1. 创建上下文
  Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  Ctx := Lib.CreateContext(sslCtxClient);

  // 2. 启用 Early Data
  if Supports(Ctx, ISSLEarlyDataContext, EarlyDataCtx) then
  begin
    EarlyDataCtx.SetClientEarlyDataEnabled(True);
    WriteLn('Early Data enabled');
  end
  else
  begin
    WriteLn('Early Data not supported by this backend');
    Exit;
  end;

  // 3. 创建连接
  Conn := Ctx.CreateConnection(Socket);

  // 4. 排队 Early Data
  if Supports(Conn, ISSLEarlyDataConnection, EarlyDataConn) then
  begin
    Request := TEncoding.UTF8.GetBytes('GET / HTTP/1.1'#13#10#13#10);
    EarlyDataConn.SetEarlyData(Request);
  end;

  // 5. 握手（Early Data 会自动发送）
  Conn.Connect;

  // 6. 检查 Early Data 状态
  case EarlyDataConn.GetEarlyDataStatus of
    sslEarlyDataAccepted:
      WriteLn('Early Data accepted by server');
    sslEarlyDataRejected:
      WriteLn('Early Data rejected, will retry in normal data');
  else
    WriteLn('Early Data not queued or not accepted');
  end;
end;
```

### 服务端 Early Data

```pascal
uses fafafa.ssl;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  EarlyDataCtx: ISSLEarlyDataContext;
begin
  // 1. 创建服务端上下文
  Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  Ctx := Lib.CreateContext(sslCtxServer);

  // 2. 配置 Early Data 策略
  if Supports(Ctx, ISSLEarlyDataContext, EarlyDataCtx) then
  begin
    // 设置策略：接受 Early Data
    EarlyDataCtx.SetServerEarlyDataPolicy(sslEarlyDataServerAccept);

    // 设置最大 Early Data 大小（16KB）
    EarlyDataCtx.SetServerMaxEarlyDataSize(16384);

    WriteLn('Server Early Data configured');
  end;

  // 3. 加载证书和密钥
  Ctx.LoadCertificate('server.crt');
  Ctx.LoadPrivateKey('server.key');

  // 4. 接受连接...
end;
```

---

## 服务端策略

### sslEarlyDataServerReject（默认）

**最安全**：拒绝所有 Early Data

```pascal
EarlyDataCtx.SetServerEarlyDataPolicy(sslEarlyDataServerReject);
```

**适用场景**：

- 不需要 Early Data 性能优化
- 安全要求高
- 无法处理重放攻击

### sslEarlyDataServerAccept（实验性）

**高性能**：接受 Early Data

```pascal
EarlyDataCtx.SetServerEarlyDataPolicy(sslEarlyDataServerAccept);
```

**适用场景**：

- 需要最低延迟
- 应用层有重放防护
- 仅处理幂等操作

**重要**：必须实现应用层重放防护！

### sslEarlyDataServerIssueOnly

**折中方案**：签发支持 Early Data 的票据，但不接受 Early Data

```pascal
EarlyDataCtx.SetServerEarlyDataPolicy(sslEarlyDataServerIssueOnly);
```

**适用场景**：

- 为未来启用 Early Data 做准备
- 测试客户端兼容性

---

## 安全最佳实践

### 1. 仅用于幂等操作

✅ **安全的操作**：

- HTTP GET 请求
- 只读 API 调用
- 查询操作

❌ **不安全的操作**：

- HTTP POST/PUT/DELETE
- 状态改变操作
- 支付、转账等

### 2. 应用层重放防护

```pascal
// 示例：使用 nonce 防止重放
type
  TEarlyDataRequest = record
    Nonce: string;      // 唯一标识符
    Timestamp: Int64;   // 时间戳
    Data: string;       // 实际数据
  end;

// 服务端验证
function ValidateEarlyData(const ARequest: TEarlyDataRequest): Boolean;
begin
  // 1. 检查 nonce 是否已使用
  if NonceCache.Contains(ARequest.Nonce) then
    Exit(False);  // 重放攻击

  // 2. 检查时间戳（5秒窗口）
  if Abs(Now - ARequest.Timestamp) > 5 then
    Exit(False);  // 过期

  // 3. 记录 nonce
  NonceCache.Add(ARequest.Nonce);

  Result := True;
end;
```

### 3. 限制 Early Data 大小

```pascal
// 限制为 4KB（足够 HTTP 请求头）
EarlyDataCtx.SetServerMaxEarlyDataSize(4096);
```

### 4. 监控和日志

```pascal
// 记录 Early Data 使用情况
case EarlyDataConn.GetEarlyDataStatus of
  sslEarlyDataAccepted:
    Log.Info('Early Data accepted');
  sslEarlyDataRejected:
    Log.Warning('Early Data rejected - possible replay attack');
end;
```

---

## 性能优化

### 1. 会话恢复

Early Data 需要会话票据：

```pascal
// 启用会话缓存
Ctx.SetSessionCacheMode(True);
Ctx.SetSessionTimeout(3600);  // 1小时
```

### 2. 预连接

```pascal
// 首次连接：建立会话
FirstConn := Ctx.CreateConnection(Socket1);
FirstConn.Connect;
FirstConn.Close;

// 后续连接：使用 Early Data
SecondConn := Ctx.CreateConnection(Socket2);
if Supports(SecondConn, ISSLEarlyDataConnection, EarlyDataConn) then
  EarlyDataConn.SetEarlyData(Request);
SecondConn.Connect;  // 快速！
```

### 3. 批量请求

```pascal
// 在 Early Data 中发送多个请求
var
  Requests: TBytes;
begin
  Requests := CombineRequests([
    'GET /api/user HTTP/1.1',
    'GET /api/settings HTTP/1.1'
  ]);
  EarlyDataConn.SetEarlyData(Requests);
end;
```

---

## 故障排查

### Early Data 未发送

**原因**：

- 没有会话票据（首次连接）
- 服务端不支持 Early Data
- 票据已过期

**解决**：

```pascal
// 检查状态
if EarlyDataConn.GetEarlyDataStatus = sslEarlyDataNone then
  WriteLn('No usable early-data session is currently configured');
```

### Early Data 被拒绝

**原因**：

- 服务端策略为 Reject
- 检测到重放攻击
- 超过大小限制

**解决**：

```pascal
// 自动重试
if EarlyDataConn.GetEarlyDataStatus = sslEarlyDataRejected then
begin
  // 数据会在正常握手后重新发送
  WriteLn('Early Data rejected, retrying in normal flow');
end;
```

### 性能未提升

**检查**：

1. 确认使用会话恢复
2. 测量网络延迟（RTT）
3. 确认服务端接受 Early Data

```pascal
// 测量延迟
StartTime := Now;
Conn.Connect;
Latency := MilliSecondsBetween(Now, StartTime);
WriteLn('Connection latency: ', Latency, 'ms');
```

---

## 完整示例

参见：

- `examples/early_data_client.pas` - 客户端示例
- `examples/early_data_server.pas` - 服务端示例
- `examples/early_data_http.pas` - HTTP 应用示例

---

## 参考资料

- [RFC 8446 - TLS 1.3](https://tools.ietf.org/html/rfc8446)
- [RFC 8470 - Using Early Data in HTTP](https://tools.ietf.org/html/rfc8470)
- [OWASP - TLS 1.3 Security](https://owasp.org/www-community/vulnerabilities/TLS_1.3_Early_Data)

---

**更新时间**: 2026-05-02
**版本**: v1.4.1
