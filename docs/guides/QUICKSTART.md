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

> WinSSL 的 `ISSLSessionResumption` public surface 已可用，但当前 dedicated Windows CI runtime truth 仍可能是
> `observed_reuse=false / session_configured=true`。
> 因此这里的示例更适合作为 API 用法参考，而不是“已 runtime-proven 会稳定命中 resumed handshake”的承诺。

这里之所以回到 direct `ISSLConnection`，是因为当前 public session-resumption surface 通过 `ISSLSessionResumption` 挂在连接对象上；普通 HTTPS 客户端仍优先走前面的 `TSSLConnector` + `TSSLStream` 快速路径。

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
  SessionResumption1, SessionResumption2: ISSLSessionResumption;
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
  (Conn1 as ISSLClientConnection).SetServerName('api.example.com');

  if Conn1.Connect and Supports(Conn1, ISSLSessionResumption, SessionResumption1) then
  begin
    WriteLn('第一次连接成功');

    // 保存 Session 供后续使用
    Session := SessionResumption1.GetSession;
    if Assigned(Session) then
      WriteLn('Session ID: ', Session.GetID);

    // 执行业务逻辑...
    Conn1.Shutdown;
  end;

  // 第二次连接 - 复用 Session
  Socket2 := ConnectToHost('api.example.com', 443);
  Conn2 := Ctx.CreateConnection(Socket2);
  if Supports(Conn2, ISSLSessionResumption, SessionResumption2) and
     Assigned(Session) and Session.IsValid and Session.IsResumable then
    SessionResumption2.SetSession(Session);  // 设置之前保存的 Session
  (Conn2 as ISSLClientConnection).SetServerName('api.example.com');

  if Conn2.Connect then
  begin
    WriteLn('第二次连接成功');

    // 检查是否复用了 Session
    if Supports(Conn2, ISSLSessionResumption, SessionResumption2) and
       SessionResumption2.IsSessionReused then
      WriteLn('当前连接命中了 resumed handshake')
    else
      WriteLn('当前 dedicated Windows CI runtime truth 仍可能是 observed_reuse=false / session_configured=true');

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
  SessionResumption: ISSLSessionResumption;
  Session: ISSLSession;
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

      // 尝试复用缓存的 Session
      if SessionCache.ContainsKey(Host) and
         Supports(Conn, ISSLSessionResumption, SessionResumption) then
      begin
        Session := SessionCache[Host];
        if Assigned(Session) and Session.IsValid and Session.IsResumable then
          SessionResumption.SetSession(Session);
      end;

      (Conn as ISSLClientConnection).SetServerName(Host);

      if Conn.Connect then
      begin
        if Supports(Conn, ISSLSessionResumption, SessionResumption) then
        begin
          if SessionResumption.IsSessionReused then
            WriteLn(Format('连接到 %s: 当前握手命中了 resumed path', [Host]))
          else
            WriteLn(Format('连接到 %s: 当前 truth 仍可能是 observed_reuse=false / session_configured=true', [Host]));

          // 保存 Session 供后续使用
          Session := SessionResumption.GetSession;
          if Assigned(Session) then
            SessionCache.AddOrSetValue(Host, Session);
        end;

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
- 当前 dedicated Windows CI runtime truth 仍可能是 `observed_reuse=false / session_configured=true`
- Session 默认有效期约 10 小时（由 Windows 系统策略控制）
- 适合 REST API 客户端、爬虫等频繁连接场景
- Session 数据较小（< 1KB），可安全缓存大量 Session
- 新代码优先通过 `ISSLSessionResumption.GetSession / SetSession / IsSessionReused` 访问会话恢复能力

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
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
