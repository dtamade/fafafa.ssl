# 将 `fafafa.ssl` 集成到你的网络通讯框架

`fafafa.ssl` 是一个 TLS 库：负责握手、加密/解密、证书与验证。

它 **不是** 网络通讯框架：

- 不负责 TCP connect/listen/accept
- 不负责 DNS 解析
- 不负责 HTTP 协议
- 不提供 event loop

你的网络层拥有并管理传输层（socket/stream）。`fafafa.ssl` 的定位是：在一个**已建立的传输之上跑 TLS**。

如果你想看一个可复用的 TCP 示例实现，可以参考 `examples/fafafa.examples.tcp.pas`（仅用于 examples，不属于库本体）。

---

## 先选一个接入面

### 选项 A：直接用 `ISSLConnection`（更适合 event loop）

当你需要：

- 手动驱动握手（尤其是非阻塞）
- 自己控制重试、超时、调度

建议直接用 `ISSLConnection`：

- 每条传输创建一个 `ISSLConnection`
- 多条连接复用同一个 `ISSLContext`
- SNI/hostname 一定要在**连接级**设置（不要写进共享 context）

### 选项 B：用 `TSSLConnector/TSSLAcceptor` + `TSSLStream`（更适合阻塞流程）

当你需要：

- “连接一次就开始读写”的顺滑入口，或
- 上层协议已经以 `TStream` 作为抽象

可以用 `TSSLConnector/TSSLAcceptor` 和 `TSSLStream`。

注意：`TSSLStream` 只是把 `ISSLConnection` 包成 `TStream`，它仍然**不拥有**你的 socket；传输层的关闭还是你上层来做。

---

## 一次构建 Context，多次创建 Connection

大多数场景建议用 `fafafa.ssl.context.builder` 构建 context（示例：client）：

```pascal
uses
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;
end;
```

之后复用 `Ctx`，为每个 socket/stream 创建一个新的 `ISSLConnection`。

---

## Socket 传输接入

### Client（阻塞握手）

前置条件：`YourConnectedSocket` 由你自己的网络代码创建并已连接到 `host:port`。

```pascal
uses
  SysUtils,
  fafafa.ssl;

var
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
begin
  Conn := Ctx.CreateConnection(YourConnectedSocket);

  // SNI + hostname verification 是连接级配置。
  // 不要把 hostname 放在共享 ISSLContext（该路径已 deprecated）。
  ClientConn := Conn as ISSLClientConnection;
  ClientConn.SetServerName('example.com');

  Conn.SetTimeout(15000);
  Conn.SetBlocking(True);

  if not Conn.Connect then
    raise Exception.Create('TLS handshake failed: ' + Conn.GetVerifyResultString);
end;
```

### Client（非阻塞握手驱动）

这段结构用于 event loop 集成。`WaitSocketReadable/WaitSocketWritable` 是伪代码，你需要用自己的 poll/epoll/kqueue/IOCP 去实现。

```pascal
uses
  SysUtils,
  fafafa.ssl.base;

var
  State: TSSLHandshakeState;
begin
  Conn.SetBlocking(False);

  while True do
  begin
    State := Conn.DoHandshake;
    case State of
      sslHsCompleted:
        Break;
      sslHsInProgress:
        begin
          if Conn.WantRead then
            WaitSocketReadable(YourConnectedSocket, Conn.GetTimeout);
          if Conn.WantWrite then
            WaitSocketWritable(YourConnectedSocket, Conn.GetTimeout);
        end;
    else
      raise Exception.Create('TLS handshake failed: ' + Conn.GetVerifyResultString);
    end;
  end;
end;
```

要点：

- `WantRead/WantWrite` 描述 TLS 层“希望底层传输满足的就绪条件”。
- 超时与取消最好由你的框架统一管理。`Conn.SetTimeout` 是连接级配置，但上层仍然应该负责 timer/cancel。

### 非阻塞读写的常见处理方式

读写通常和握手一样处理：

```pascal
var
  R: Integer;
  Err: TSSLErrorCode;
begin
  R := Conn.Read(Buffer, BufferSize);
  if R < 0 then
  begin
    Err := Conn.GetError(R);
    if Err = sslErrWouldBlock then
    begin
      if Conn.WantRead then WaitSocketReadable(YourConnectedSocket, Conn.GetTimeout);
      if Conn.WantWrite then WaitSocketWritable(YourConnectedSocket, Conn.GetTimeout);
    end
    else
      raise Exception.Create('TLS read failed');
  end;
end;
```

如果你用的是 `TSSLStream`，读写失败会抛异常（而不是返回 `-1`）。

### Shutdown 与“谁关闭 socket”

`fafafa.ssl` 不会帮你关闭 socket。

- `Conn.Shutdown`：尝试做 TLS 的优雅关闭（close_notify）
- socket 的真正关闭：由你的网络层完成

---

## `TStream` 传输接入

当你的网络层已经抽象出了一个“可读写的 duplex stream”，可以直接用 `ConnectStream`：

```pascal
uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.tls;

var
  TLS: TSSLConnector;
  Stream: TSSLStream;
begin
  TLS := TSSLConnector.FromContext(Ctx).WithTimeout(15000);
  Stream := TLS.ConnectStream(YourDuplexStream, 'example.com');
  try
    // Stream.Read / Stream.Write
  finally
    Stream.Free;
  end;
end;
```

你的 `TStream` 仍然负责底层连接的生命周期（打开/关闭/超时/取消）。

---

## 用 ALPN 做 TLS 级协议协商

ALPN 由 TLS 协商，但“协商后要做什么”（例如 HTTP/2 vs HTTP/1.1）属于你上层协议栈的范畴。

运行：

```pascal
Ctx.SetALPNProtocols('h2,http/1.1');
```

握手成功后：

```pascal
WriteLn('Selected ALPN: ', Conn.GetSelectedALPNProtocol);
```

---

## 排错与诊断

握手失败时，优先看这些：

- `Conn.GetVerifyResult` / `Conn.GetVerifyResultString`（证书验证结果）
- `Conn.GetProtocolVersion` / `Conn.GetCipherName`
- `Conn.GetStateString`（后端相关的状态描述）

---

## 值得直接抄的示例

- `examples/01_tls_client.pas`
- `examples/https_simple_get.pas`
- `examples/https_client/README.md`
- `examples/fafafa.examples.tcp.pas`（TCP helper，仅用于示例）
