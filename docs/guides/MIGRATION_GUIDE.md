# fafafa.ssl 迁移指南

> **版本**: rolling
> **最后更新**: 2026-05-19
> **当前路线图**: [当前路线图](../ROADMAP.md)
> **说明**: 当前迁移真相以 `src/fafafa.ssl.base.pas`、`src/fafafa.ssl.pas`、`src/fafafa.ssl.tls.pas` 和 `docs/reference/API_REFERENCE.md` 为准。

预 `v1.0` 的历史变更只适合帮助你阅读旧代码，不应再被当成当前 active public API 说明。

## 先抓当前迁移心智

迁移到当前 `fafafa.ssl` 时，先记住这四条：

1. 新代码优先走当前公开门面：`fafafa.ssl`、`fafafa.ssl.context.builder`、`TSSLFactory`、`TSSLConnector`、`TSSLStream`。
2. 连接语义已经固定成 transport-first：先拿到底层 socket/stream，再进入 TLS。
3. client SNI/hostname 是连接级配置，不再推荐写在共享 context 上。
4. 可选能力要以后端 capability truth 为准，不要默认所有 backend 都完全同构。

`TSSLConfig.ServerName` / `ISSLContext.SetServerName(...)` / `TSSLContextBuilder.WithSNI(...)` 当前都只应视为 compatibility-only 入口。

## 从旧版 fafafa.ssl 迁移

如果你在读预 `v1.0` 代码，最常见的迁移点是这几条：

- 旧的 `fafafa.ssl.abstract.intf` 已经收进当前公开接口面；新代码直接使用 `fafafa.ssl` 或 `fafafa.ssl.base`。
- client hostname/SNI 不再推荐存进 context；改成每条连接自己带上 hostname。
- 如果你想少写样板代码，优先从 `TSSLConnector` / `TSSLStream` 入手，而不是直接围绕 backend 单元做初始化。
- 如果你必须继续使用原始 `ISSLConnection`，也要显式走 `ISSLClientConnection.SetServerName(...)`。

## 从 Synapse 迁移

Synapse 常见写法是“socket + SSL handler 混在一个对象里”。迁到当前 `fafafa.ssl`，更接近的心智是：

- transport 仍然由你自己控制
- TLS 上下文单独配置
- `TSSLConnector` 负责把已连接 socket 升级成 TLS stream

**Synapse**:

```pascal
uses ssl_openssl;

var
  LSocket: TTCPBlockSocket;
begin
  LSocket := TTCPBlockSocket.Create;
  try
    LSocket.SSL.SSLType := LT_TLSv1_2;
    LSocket.SSL.CertificateFile := 'cert.pem';
    LSocket.SSL.PrivateKeyFile := 'key.pem';
    LSocket.Connect('example.com', '443');
  finally
    LSocket.Free;
  end;
end;
```

**当前推荐迁移写法**:

```pascal
uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  LSocket: TSocket;
  LContext: ISSLContext;
  LTLS: TSSLStream;
begin
  LSocket := ConnectToServer('example.com', 443); // 你的 transport helper

  LContext := TSSLContextBuilder.Create
    .WithBackend(sslOpenSSL)
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  LTLS := TSSLConnector.FromContext(LContext).ConnectSocket(THandle(LSocket), 'example.com');
  try
    // 通过 LTLS.Read / LTLS.Write 进行 TLS I/O
  finally
    LTLS.Free;
  end;
end;
```

如果你要保留更接近原始 `ISSLConnection` 的控制方式，可以这样写：

```pascal
var
  LConn: ISSLConnection;
  LResponse: string;
begin
  LConn := LContext.CreateConnection(THandle(LSocket));
  (LConn as ISSLClientConnection).SetServerName('example.com');

  if LConn.Connect then
  begin
    LConn.WriteString('GET / HTTP/1.1'#13#10'Host: example.com'#13#10#13#10);
    if LConn.ReadString(LResponse) then
      WriteLn(LResponse);
  end;
end;
```

这段 direct `ISSLConnection` 写法仍是当前 shipped surface；如果你只是做框架/transport 集成，优先走 `TSSLStream` 或 `Read` / `Write`，`WriteString` 继续作为 `v1.x` convenience-core 文本 helper 保留。

## 从 Indy 迁移

Indy 旧代码通常把 TLS 细节藏在 `IOHandler` 里。迁到当前 `fafafa.ssl` 时，建议把“HTTP 客户端”和“TLS 升级”拆开理解。

**Indy**:

```pascal
uses IdSSLOpenSSL, IdHTTP;

var
  LHttp: TIdHTTP;
  LHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  LHttp := TIdHTTP.Create(nil);
  LHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    LHandler.SSLOptions.Method := sslvTLSv1_2;
    LHttp.IOHandler := LHandler;
    LHttp.Get('https://example.com');
  finally
    LHttp.Free;
    LHandler.Free;
  end;
end;
```

**当前推荐迁移写法**:

```pascal
uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  LSocket: TSocket;
  LContext: ISSLContext;
  LTLS: TSSLStream;
  LResponse: string;
begin
  LSocket := ConnectToServer('example.com', 443); // 你的 transport helper

  LContext := TSSLContextBuilder.Create
    .WithBackend(sslOpenSSL)
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  LTLS := TSSLConnector.FromContext(LContext).WithTimeout(15000)
    .ConnectSocket(THandle(LSocket), 'example.com');
  try
    LTLS.WriteBuffer(Pointer('GET / HTTP/1.1'#13#10'Host: example.com'#13#10#13#10)^, 37);
    SetLength(LResponse, 0);
    // 你的 HTTP 读取逻辑
  finally
    LTLS.Free;
  end;
end;
```

## 从 OpenSSL C API 迁移

如果你原来直接写 `SSL_CTX_new` / `SSL_new` / `SSL_connect`，迁移时最重要的变化不是“方法名换了”，而是：

- 证书、协议、验证参数进 `ISSLContext`
- 连接级 hostname/SNI 进 `TSSLConnector` 或 `ISSLClientConnection`
- 读写可以走 `TSSLStream`，也可以继续走 `ISSLConnection`

**OpenSSL C API**:

```c
SSL_CTX *ctx = SSL_CTX_new(TLS_method());
SSL_CTX_use_certificate_file(ctx, "cert.pem", SSL_FILETYPE_PEM);
SSL_CTX_use_PrivateKey_file(ctx, "key.pem", SSL_FILETYPE_PEM);

SSL *ssl = SSL_new(ctx);
SSL_set_fd(ssl, socket_fd);
SSL_connect(ssl);
```

**当前推荐迁移写法**:

```pascal
uses
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  LContext: ISSLContext;
  LTLS: TSSLStream;
begin
  LContext := TSSLContextBuilder.Create
    .WithBackend(sslOpenSSL)
    .WithTLS12And13
    .WithVerifyPeer
    .WithCertificate('cert.pem')
    .WithPrivateKey('key.pem')
    .BuildClient;

  LTLS := TSSLConnector.FromContext(LContext).ConnectSocket(THandle(SocketFD), 'example.com');
  try
    // 通过 LTLS 读写
  finally
    LTLS.Free;
  end;
end;
```

## WinSSL 企业 helper 的当前边界

如果你在 Windows 上迁移企业场景，`fafafa.ssl.winssl.enterprise` 仍然可以用，但要按当前 shipped 命名来理解。

`TSSLEnterpriseConfig` 当前 helper 名称是 `IsFIPSEnabled`、`GetTrustedRoots`、`GetAllPolicies`。
`IsFIPSEnabled` 代表系统 policy/helper 检测，不等于当前 WinSSL backend 已发布 `SupportsFIPSMode=True`。

```pascal
{$IFDEF WINDOWS}
uses
  fafafa.ssl.winssl.enterprise;

var
  LConfig: TSSLEnterpriseConfig;
begin
  LConfig := TSSLEnterpriseConfig.Create;
  try
    LConfig.LoadFromSystem;
    if LConfig.IsFIPSEnabled then
      WriteLn('FIPS mode enabled');
    WriteLn('Trusted roots: ', Length(LConfig.GetTrustedRoots));
    WriteLn('Policies loaded: ', LConfig.GetAllPolicies.Count);
  finally
    LConfig.Free;
  end;
end;
{$ENDIF}
```

这组 helper 仍然是 WinSSL-specific surface，不应被写成所有 backend 共有能力。

## OpenSSL 低层错误 helper 的当前边界

`GetFriendlyErrorMessage(...)` / `GetOpenSSLErrorCategory(...)` 当前来自 `fafafa.ssl.openssl.api.err`，属于 OpenSSL-specific low-level helper，不是通用 public facade API。

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.api.err;

var
  LLib: ISSLLibrary;
  LError: Cardinal;
begin
  LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  LError := LLib.GetLastError;
  WriteLn(GetFriendlyErrorMessage(LError));
  WriteLn(GetOpenSSLErrorCategory(LError));
end;
```

如果你在写跨 backend 的迁移代码，优先依赖：

- `ISSLLibrary.GetLastError`
- `ISSLLibrary.GetLastErrorString`
- `ISSLLibrary.GetCapabilities`
- `Supports(...)`

不要默认把 OpenSSL 的低层错误分类 helper 当成所有 backend 的统一接口。

## 迁移检查清单

- 使用当前公开单元，而不是旧的 `abstract.intf` 或不存在的 facade 单元。
- client 连接在握手前明确带上 hostname/SNI。
- transport 与 TLS 分层清楚：先 socket/stream，再 `CreateConnection` 或 `TSSLConnector.Connect*`。
- optional capability 先看 `ISSLLibrary.GetCapabilities`，再决定是否使用 backend-specific surface。
- 如果你还在照旧 release 文档迁移，先回到 [API_REFERENCE.md](../reference/API_REFERENCE.md) 和 [ROADMAP.md](../ROADMAP.md) 重新对齐。

## 更多资源

- [API 参考文档](../reference/API_REFERENCE.md)
- [故障排查指南](./TROUBLESHOOTING.md)
- [WinSSL 用户指南](./WINSSL_USER_GUIDE.md)
- [示例代码](../../examples/)
