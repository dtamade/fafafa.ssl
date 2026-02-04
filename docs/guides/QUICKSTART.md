# fafafa.ssl 快速开始（Quickstart）

本指南用于 **5 分钟**内跑通：
- 密码学工具（SHA-256 / 安全随机数）
- TLS 客户端握手（Connector + Stream）

fafafa.ssl 是 **TLS 库**，不是 HTTP 库。

## 1) 安装与编译

### 依赖
- FreePascal 3.2.0+
- Linux/macOS: OpenSSL 1.1.1+ 或 3.x（运行时动态加载）
- Windows: 默认可用 WinSSL（Schannel），无需 OpenSSL DLL

### 本地编译
```bash
# 在仓库根目录
fpc -Mobjfpc -Sh -Fu./src -Fi./src your_program.pas
```

## 2) 密码学工具（高层封装）

```pascal
program quick_crypto;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.crypto.utils;

var
  Bytes: TBytes;
  Hex: string;
begin
  Hex := TCryptoUtils.SHA256Hex('Hello');
  WriteLn('SHA256: ', Hex);

  Bytes := TCryptoUtils.SecureRandom(32);
  WriteLn('Random length: ', Length(Bytes));
end.
```

## 3) TLS 客户端（Connector + Stream）

> 你需要自己创建并连接 TCP socket（或使用 Synapse/Indy/lNet 等网络库）。
> fafafa.ssl 的职责是：在“已连接的传输”之上提供 TLS。

```pascal
program quick_tls;

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
  // 1) Context：协议 + 验证 + 系统根证书
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 2) Connector：把 ServerName 设置到“连接”上（SNI + hostname verification）
  TLS := TSSLConnector.FromContext(Ctx);
  Stream := TLS.ConnectSocket(YourConnectedSocket, 'example.com');
  try
    WriteLn('TLS OK, cipher: ', Stream.Connection.GetCipherName);
    // Stream.Read/Write ...
  finally
    Stream.Free;
  end;
end.
```

### 3.1) WinSSL Session 复用（Windows 平台）

> WinSSL 后端支持 TLS Session 复用，可显著提升连接性能（70-90%）。
> 适用于需要频繁连接同一服务器的场景（如 REST API 客户端）。

**基本 Session 复用示例**:
```pascal
program winssl_session_reuse;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  Conn1, Conn2: ISSLConnection;
  Session: ISSLSession;
  Socket1, Socket2: THandle;
begin
  // 创建 WinSSL 上下文
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL)
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 第一次连接 - 完整握手
  Socket1 := ConnectToHost('api.example.com', 443);
  Conn1 := Ctx.CreateConnection(Socket1);
  Conn1.SetServerName('api.example.com');

  if Conn1.Connect then
  begin
    WriteLn('第一次连接成功');
    WriteLn('Session ID: ', Conn1.GetSessionID);

    // 保存 Session 供后续使用
    Session := Conn1.GetSession;

    // 执行业务逻辑...
    Conn1.Shutdown;
  end;

  // 第二次连接 - 复用 Session
  Socket2 := ConnectToHost('api.example.com', 443);
  Conn2 := Ctx.CreateConnection(Socket2);
  Conn2.SetServerName('api.example.com');
  Conn2.SetSession(Session);  // 设置之前保存的 Session

  if Conn2.Connect then
  begin
    WriteLn('第二次连接成功');

    // 检查是否复用了 Session
    if Conn2.IsSessionResumed then
      WriteLn('✓ Session 复用成功 - 握手时间大幅减少')
    else
      WriteLn('✗ Session 未复用 - 执行了完整握手');

    Conn2.Shutdown;
  end;
end.
```

**多主机 Session 缓存示例**:
```pascal
program winssl_session_cache;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes, Generics.Collections,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  SessionCache: TDictionary<string, ISSLSession>;
  Conn: ISSLConnection;
  Host: string;
  Hosts: TStringList;
  Socket: THandle;
begin
  // 创建 WinSSL 上下文
  Ctx := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL)
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 创建 Session 缓存
  SessionCache := TDictionary<string, ISSLSession>.Create;
  Hosts := TStringList.Create;
  try
    Hosts.Add('api.example.com');
    Hosts.Add('cdn.example.com');
    Hosts.Add('www.example.com');

    // 连接到多个主机
    for Host in Hosts do
    begin
      Socket := ConnectToHost(Host, 443);
      Conn := Ctx.CreateConnection(Socket);
      Conn.SetServerName(Host);

      // 尝试复用缓存的 Session
      if SessionCache.ContainsKey(Host) then
        Conn.SetSession(SessionCache[Host]);

      if Conn.Connect then
      begin
        WriteLn(Format('连接到 %s: Session %s',
          [Host,
           IfThen(Conn.IsSessionResumed, '复用', '新建')]));

        // 保存 Session 供后续使用
        SessionCache.AddOrSetValue(Host, Conn.GetSession);

        // 执行业务逻辑...
        Conn.Shutdown;
      end;
    end;
  finally
    SessionCache.Free;
    Hosts.Free;
  end;
end.
```

**性能提示**:
- Session 复用可减少 70-90% 的握手时间
- Session 默认有效期约 10 小时（由 Windows 系统策略控制）
- 适合 REST API 客户端、爬虫等频繁连接场景
- Session 数据较小（< 1KB），可安全缓存大量 Session

---

## 4) 证书快速生成（Quick API）

```pascal
program quick_cert;

{$mode ObjFPC}{$H+}

uses
  fafafa.ssl.quick,
  fafafa.ssl.cert.builder;

var
  KeyPair: IKeyPairWithCertificate;
begin
  KeyPair := TSSLQuick.GenerateSelfSigned('localhost');
  KeyPair.SaveToFiles('server.crt', 'server.key');
end.
```

## 5) 下一步

- 示例：`examples/`
- 构建与测试：
  - `bash build_linux.sh`
  - `./ci_pipeline.sh build`
  - `./ci_pipeline.sh test`
