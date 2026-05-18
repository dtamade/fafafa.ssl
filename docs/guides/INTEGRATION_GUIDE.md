# fafafa.ssl 框架集成指南

本文档说明如何将 fafafa.ssl 集成到其他网络框架或自定义 I/O 模型中。

fafafa.ssl 专注于 SSL/TLS 协议处理，不涉及 socket 管理或 I/O 模型。这使得它可以轻松集成到任何网络框架中。

---

## 核心原则

### fafafa.ssl 负责什么

- ✅ SSL/TLS 握手
- ✅ 数据加密/解密
- ✅ 证书验证
- ✅ 会话管理
- ✅ 错误状态报告

### fafafa.ssl 不负责什么

- ❌ Socket 创建和管理
- ❌ I/O 多路复用 (select/poll/epoll)
- ❌ 异步 I/O 模型
- ❌ 线程池管理
- ❌ 连接超时控制

---

## 快速集成

### 1. 基本用法（同步阻塞）

```pascal
uses
  fafafa.ssl.factory, fafafa.ssl.base;

var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LSocket: TSocket;  // 你的框架提供的 socket
begin
  // 创建 SSL 上下文
  LCtx := TSSLFactory.CreateContext(sslCtxClient);
  LCtx.SetVerifyMode(sslVerifyPeer);

  // 创建 SSL 连接（包装现有 socket）
  LConn := LCtx.CreateConnection(LSocket);
  LClientConn := LConn as ISSLClientConnection;
  LClientConn.SetServerName('example.com');  // 客户端 SNI/hostname 要设在连接上

  // 执行 SSL 握手
  if LConn.Connect then
  begin
    // 通过 SSL 读写数据
    LConn.Write(Data, Length(Data));
    BytesRead := LConn.Read(Buffer, SizeOf(Buffer));
  end;

  // 关闭
  LConn.Shutdown;
end;
```

### 2. 非阻塞模式集成

对于事件驱动的框架，需要处理 `WantRead` 和 `WantWrite` 状态：

```pascal
var
  LConn: ISSLConnection;
  LHandshakeState: TSSLHandshakeState;
  LBytesWritten: Integer;
  LError: TSSLErrorCode;
begin
  // 设置 socket 为非阻塞模式（由你的框架处理）
  SetNonBlocking(LSocket, True);

  // 非阻塞握手
  repeat
    LHandshakeState := LConn.DoHandshake;

    case LHandshakeState of
      hsInProgress:
        begin
          // 检查 SSL 层需要什么
          if LConn.WantRead then
            WaitForRead(LSocket)   // 你的框架：等待可读
          else if LConn.WantWrite then
            WaitForWrite(LSocket); // 你的框架：等待可写
        end;
      hsComplete:
        Break;  // 握手完成
      hsFailed:
        raise Exception.Create('Handshake failed');
    end;
  until False;

  // 非阻塞写入
  LBytesWritten := LConn.Write(Data, Length(Data));
  if LBytesWritten < 0 then
  begin
    LError := LConn.GetError(LBytesWritten);
    case LError of
      sslErrWantWrite:
        // 需要等待 socket 可写后重试
        WaitForWrite(LSocket);
      sslErrWantRead:
        // SSL 重协商，需要等待可读
        WaitForRead(LSocket);
      else
        // 真正的错误
        HandleError(LError);
    end;
  end;
end;
```

---

## 错误状态详解

### TSSLErrorCode 关键值

| 错误码 | 含义 | 集成方处理 |
|--------|------|-----------|
| `sslErrNone` | 操作成功 | 继续 |
| `sslErrWantRead` | SSL 层需要读取数据 | 等待 socket 可读后重试 |
| `sslErrWantWrite` | SSL 层需要写入数据 | 等待 socket 可写后重试 |
| `sslErrWouldBlock` | 操作会阻塞 | 稍后重试 |
| `sslErrHandshake` | 握手失败 | 检查证书/协议配置 |
| `sslErrCertificate` | 证书问题 | 检查证书链 |
| `sslErrConnection` | 连接已断开 | 关闭并清理 |

### 非阻塞操作返回值

```pascal
// Read 返回值
BytesRead > 0   : 成功读取数据
BytesRead = 0   : 对端关闭连接
BytesRead < 0   : 需要调用 GetError() 获取详细状态

// Write 返回值
BytesWritten > 0 : 成功写入数据
BytesWritten = 0 : 无法写入（缓冲区满或其他原因）
BytesWritten < 0 : 需要调用 GetError() 获取详细状态
```

---

## ISSLConnection 接口速查

### 核心方法

```pascal
ISSLConnection = interface
  // 连接控制
  function Connect: Boolean;           // 客户端连接（含握手）
  function Accept: Boolean;            // 服务端接受（含握手）
  function DoHandshake: TSSLHandshakeState;  // 非阻塞握手
  function Shutdown: Boolean;          // 优雅关闭
  procedure Close;                     // 强制关闭

  // 数据传输
  function Read(var ABuffer; ACount: Integer): Integer;
  function Write(const ABuffer; ACount: Integer): Integer;

  // 非阻塞状态
  function WantRead: Boolean;          // SSL 需要读取？
  function WantWrite: Boolean;         // SSL 需要写入？
  function GetError(ARet: Integer): TSSLErrorCode;

  // 连接信息
  function IsHandshakeComplete: Boolean;
  function GetProtocolVersion: TSSLProtocolVersion;
  function GetCipherName: string;
  function GetPeerCertificate: ISSLCertificate;
end;
```

### 握手状态

```pascal
TSSLHandshakeState = (
  hsNotStarted,   // 未开始
  hsInProgress,   // 进行中（非阻塞模式下需要重试）
  hsComplete,     // 已完成
  hsFailed        // 失败
);
```

---

## 常见集成场景

### 场景 1: Indy 集成

```pascal
type
  TIndySSLIOHandler = class(TIdIOHandler)
  private
    FSSLConn: ISSLConnection;
  public
    function Readable(ATimeout: Integer): Boolean; override;
    function Recv(var VBuffer: TIdBytes): Integer; override;
    function Send(const ABuffer: TIdBytes): Integer; override;
  end;

function TIndySSLIOHandler.Recv(var VBuffer: TIdBytes): Integer;
var
  LError: TSSLErrorCode;
begin
  Result := FSSLConn.Read(VBuffer[0], Length(VBuffer));
  if Result < 0 then
  begin
    LError := FSSLConn.GetError(Result);
    if LError in [sslErrWantRead, sslErrWantWrite] then
      Result := 0  // 告诉 Indy 稍后重试
    else
      raise EIdException.Create('SSL Read error');
  end;
end;
```

### 场景 2: Synapse 集成

```pascal
type
  TSSLSynapsePlugin = class(TCustomSSL)
  private
    FSSLConn: ISSLConnection;
  public
    function Connect: Boolean; override;
    function Shutdown: Boolean; override;
    function BiShutdown: Boolean; override;
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;
  end;
```

### 场景 3: 自定义事件循环

```pascal
procedure TMyEventLoop.HandleSSLConnection(AConn: TMyConnection);
var
  LSSLConn: ISSLConnection;
  LState: TSSLHandshakeState;
  LBytes: Integer;
begin
  LSSLConn := AConn.SSLConnection;

  // 握手阶段
  if not LSSLConn.IsHandshakeComplete then
  begin
    LState := LSSLConn.DoHandshake;
    case LState of
      hsInProgress:
        begin
          if LSSLConn.WantRead then
            RegisterForRead(AConn)
          else if LSSLConn.WantWrite then
            RegisterForWrite(AConn);
        end;
      hsComplete:
        AConn.OnHandshakeComplete;
      hsFailed:
        AConn.OnError;
    end;
    Exit;
  end;

  // 数据传输阶段
  if AConn.HasDataToSend then
  begin
    LBytes := LSSLConn.Write(AConn.SendBuffer^, AConn.SendSize);
    if LBytes > 0 then
      AConn.AdvanceSendBuffer(LBytes)
    else if LSSLConn.WantWrite then
      RegisterForWrite(AConn);
  end;

  if AConn.CanRead then
  begin
    LBytes := LSSLConn.Read(AConn.RecvBuffer^, AConn.RecvCapacity);
    if LBytes > 0 then
      AConn.OnDataReceived(LBytes)
    else if LSSLConn.WantRead then
      RegisterForRead(AConn);
  end;
end;
```

---

## 线程安全

### 单连接单线程

每个 `ISSLConnection` 实例应该由单个线程操作。如果需要在多线程间传递，确保：

1. 同一时间只有一个线程操作连接
2. 使用锁保护共享访问

### 上下文可共享

`ISSLContext` 是线程安全的，可以在多线程间共享：

```pascal
var
  GSharedCtx: ISSLContext;  // 全局共享

// 在不同线程中
LConn := GSharedCtx.CreateConnection(LSocket);  // 安全
```

### 会话缓存

内置的 `TShardedSessionCache` 是线程安全的：

```pascal
// 自动会话复用，无需额外处理
LCtx.SetSessionCacheMode(scmClient);
```

---

## 性能优化建议

### 1. 复用 SSL 上下文

```pascal
// 不好：每次连接创建新上下文
for I := 1 to 1000 do
begin
  LCtx := TSSLFactory.CreateContext(sslCtxClient);  // 开销大
  LConn := LCtx.CreateConnection(Socket);
end;

// 好：复用上下文
LCtx := TSSLFactory.CreateContext(sslCtxClient);
for I := 1 to 1000 do
begin
  LConn := LCtx.CreateConnection(Socket);  // 快速
end;
```

### 2. 启用会话复用

```pascal
LCtx.SetSessionCacheMode(scmClient);  // 客户端
// 或
LCtx.SetSessionCacheMode(scmServer);  // 服务端
```

### 3. 使用内置缓冲区池

```pascal
uses fafafa.ssl.buffer.pool;

// 获取缓冲区
LBuffer := GlobalBufferPool.Acquire(16384);
try
  BytesRead := LConn.Read(LBuffer.Data^, LBuffer.Size);
finally
  GlobalBufferPool.Release(LBuffer);
end;
```

---

## 错误处理最佳实践

### 使用 Result 类型

```pascal
var
  LResult: TSSLOperationResult;
begin
  LResult := LConn.ConnectWithResult;

  if LResult.IsOk then
    // 成功
  else
    case LResult.ErrorCode of
      sslErrCertificate:
        ShowMessage('证书验证失败: ' + LResult.ErrorMessage);
      sslErrConnection:
        ShowMessage('连接失败');
      else
        ShowMessage('错误: ' + SSLErrorToString(LResult.ErrorCode));
    end;
end;
```

### 错误日志

```pascal
uses fafafa.ssl.logging;

// 设置日志回调
TSSLLogger.SetCallback(procedure(const AMsg: string)
begin
  MyLogger.Log(AMsg);
end);

// 设置日志级别
TSSLLogger.SetLevel(sslLogDebug);
```

---

## 版本兼容

fafafa.ssl 保持向后兼容：

| 版本 | 兼容性 |
|------|--------|
| 1.x → 1.x | 完全兼容 |
| 1.x → 2.x | 接口兼容，新增功能 |

---

## 参考

- [API 参考](API_REFERENCE.md)
- [错误处理指南](ERROR_HANDLING_BEST_PRACTICES.md)
- [性能优化指南](PERFORMANCE_GUIDE.md)

---

*文档版本: 1.0*
*最后更新: 2026-02-05*
