# 🚀 fafafa.ssl 快速入门（Getting Started）

fafafa.ssl 是一个 **SSL/TLS 库**：负责握手、加密传输、证书与验证。

它 **不是 HTTP 库**。
- 你需要自己创建/管理 TCP socket（或使用你喜欢的网络库）。
- fafafa.ssl 只接管 “在已连接的传输之上跑 TLS”。

## 1) 推荐入口（2025-12-31）

### 构建 TLS 配置（Context）
使用 `fafafa.ssl.context.builder`：
- `.WithSystemRoots`：加载系统根证书（现在通过 `ISSLCertificateStore.LoadSystemStore` 实现，跨后端一致）。
- `.WithTLS12And13` / `.WithVerifyPeer`：给出安全默认。

### 建立 TLS 连接（Rust 风格门面）
使用 `fafafa.ssl.tls`：
- `TSSLConnector`：客户端
- `TSSLAcceptor`：服务端
- `TSSLStream`：把 `ISSLConnection` 封装成 `TStream`

并且 **SNI/hostname 是“连接级别”配置**：
- 使用 `ISSLClientConnection.SetServerName`（或 `TSSLConnector.Connect*(..., ServerName)`）
- 不要把 hostname 放在共享 `ISSLContext` 上（该做法已标记为 deprecated）。

## 2) 第一个例子：SHA-256

```pascal
program hash_example;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.crypto.utils;

begin
  WriteLn('SHA256: ', TCryptoUtils.SHA256Hex('Hello, fafafa.ssl!'));
end.
```

编译：
```bash
fpc -Fu./src -Fi./src hash_example.pas
```

## 3) 第一个 TLS 客户端：Connector + Stream

下面示例展示“TLS 层”如何用在你创建的 TCP socket 之上。

> 注意：`YourConnectedSocket` 需要你自己创建并连接到 `example.com:443`。
> 你可以参考 `examples/` 目录里现成的 socket 连接示例。

```pascal
program tls_client_example;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  YourConnectedSocket: THandle;

begin
  // 1) 构建客户端上下文（加载系统根证书）
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 2) 使用 Connector 建立 TLS（ServerName 用于 SNI + hostname verification）
  TLS := TSSLConnector.FromContext(Ctx);
  Stream := TLS.ConnectSocket(YourConnectedSocket, 'example.com');
  try
    WriteLn('✓ TLS OK');
    WriteLn('Protocol: ', Ord(Stream.Connection.GetProtocolVersion));
    WriteLn('Cipher: ', Stream.Connection.GetCipherName);

    // 3) 你可以把 Stream 交给任何接受 TStream 的上层协议实现
    // Stream.Write(...); Stream.Read(...);
  finally
    Stream.Free;
  end;
end.
```

## 4) 直接用 ISSLConnection（显式设置 per-connection SNI）

如果你不想用 `TSSLConnector`，也可以直接：

```pascal
var
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
begin
  Conn := Ctx.CreateConnection(YourConnectedSocket);
  ClientConn := Conn as ISSLClientConnection;
  ClientConn.SetServerName('example.com');
  if not Conn.Connect then
    raise Exception.Create('TLS handshake failed');
end;
```

## 5) 快速生成测试证书（Quick API）

```pascal
uses
  fafafa.ssl.quick, fafafa.ssl.cert.builder;

var
  KeyPair: IKeyPairWithCertificate;
begin
  KeyPair := TSSLQuick.GenerateSelfSigned('localhost');
  KeyPair.SaveToFiles('server.crt', 'server.key');
end;
```

## 6) 下一步

- 看示例：`examples/`
- 跑构建与测试：
  - `bash build_linux.sh`
  - `./ci_pipeline.sh build`
  - `./ci_pipeline.sh test`
