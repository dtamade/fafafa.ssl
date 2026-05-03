# OCSP 使用指南

本文覆盖两条不同的 OCSP 路径：

- 如果你要消费 TLS 握手里的 stapled OCSP response，先看 FreePascal client runtime 路径。
- 如果你要在 `WolfSSL` 上接通最小的 stapled-response request / manual issuance path，看第 3 节后半段的实验性说明。
- 如果你要在 FreePascal client 上启用基于证书 AIA 的 online OCSP check，看后面的 client online 路径。
- 如果你要主动构造、发送、验证 OCSP 请求，使用最后的 OpenSSL helper 工作流。

---

## 1. 先用 FreePascal client runtime OCSP stapling

当前 pure Pascal TLS 1.3 client 已经支持在握手期间请求 `status_request`，并通过连接对象暴露 stapled OCSP response surface。

最短用法是：

```pascal
uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  OCSP: ISSLOCSPStapling;
begin
  Ctx := TSSLContextBuilder.Create
    .WithVerifyPeer
    .WithOCSPStapling(True)
    .BuildClient;

  Conn := Ctx.CreateConnection(Socket);
  if not Conn.Connect then
    raise Exception.Create(Conn.GetVerifyResultString);

  if Supports(Conn, ISSLOCSPStapling, OCSP) then
  begin
    WriteLn('Stapling enabled: ', OCSP.GetOCSPStaplingEnabled);
    WriteLn('Response bytes: ', Length(OCSP.GetOCSPResponse));
    WriteLn('Verified: ', OCSP.IsOCSPResponseVerified);
    WriteLn('Status: ', OCSP.GetOCSPResponseStatus);
  end;
end;
```

这条路径的要点是：

- `WithOCSPStapling(True)` 会让 client 在握手里请求 `status_request`。
- 握手完成后，可以通过 `ISSLOCSPStapling` 读取 raw stapled response、verified bit，以及状态文本。
- 如果服务端没有返回 stapled response，可选模式下连接仍然成功，surface 会返回空响应、`verified = False`。

---

## 2. 需要 fail-closed 时，启用 required 模式

如果你的策略是“没有合格的 stapled response 就不要继续”，可以再加上 `WithOCSPStaplingRequired(True)`：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .WithOCSPStapling(True)
  .WithOCSPStaplingRequired(True)
  .BuildClient;
```

当前 FreePascal client runtime 已经锁定的语义是：

- 在 `verify-peer` 的 non-resumed full-handshake path 上：
  - 服务端缺少 stapled response 时，`Connect` 会 fail-closed。
  - 服务端给了 stapled response，但当前有界 verifier 不接受时，`Connect` 也会 fail-closed。
- 即使 response 不被接受，非 required 模式下你仍然可以通过 `ISSLOCSPStapling` 看到 raw bytes 和状态文本。

还需要明确两个当前已经锁定的 boundary：

- 如果关闭 `verify-peer`，当前实现不会因为 `WithOCSPStaplingRequired(True)` 被 fail-closed；这时它仍然只是 request/surface path。
- 对 resumed TLS 1.3 path，`required` 也不会因为 resumed flight 缺少新的 certificate / stapled response 被触发。

当前不要把这条路径理解成“完整 browser-grade revocation strategy”。当前已经收口的范围包括：

- stapled response 只有在 cryptographic verification 通过时才会 surface `Verified = True`
- `WithOCSPStaplingRequired(True)` 会对缺失或 verification failure 的 stapled response fail-closed

当前仍保留的边界是：

- 这条路径仍依赖现有 OpenSSL OCSP helper / transport hooks，而不是库内建完整网络栈
- 服务端 stapling 只负责 caller-provided DER/file material，不负责 online fetch / refresh / responder 调度

---

## 3. 在 FreePascal server 上配置 stapled response

当前 FreePascal server path 已经提供 public optional context interface 和 builder file-based 配置入口。

最短用法是：

```pascal
uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  ServerStapling: ISSLServerOCSPStaplingContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithCertificate('server.crt')
    .WithPrivateKey('server.key')
    .WithOCSPStapling(True)
    .WithServerOCSPStapledResponseFile('fixtures/ocsp/server_leaf.ocsp.der')
    .BuildServer;

  if Supports(Ctx, ISSLServerOCSPStaplingContext, ServerStapling) then
    WriteLn('Configured stapled-response bytes: ',
      Length(ServerStapling.GetServerStapledOCSPResponse));
end;
```

这条路径当前已经锁定的语义是：

- `WithServerOCSPStapledResponseFile(...)` 会在 `BuildServer` 时加载调用方提供的 DER 文件。
- `ISSLServerOCSPStaplingContext` 暴露 clear / set bytes / load file / has / get 这组最小 public surface。
- 服务端只会在 `full handshake + client requested status_request + context 已配置 stapled response` 三个条件同时满足时发出 stapled response。
- 如果 builder 配置了 `server_ocsp_stapled_response_file`，但 backend 不支持 `ISSLServerOCSPStaplingContext`，`BuildServer` 会直接报配置错误，不会 silent ignore。

还需要明确的边界是：

- 这条路径只负责 caller-provided material，不负责 online fetch、refresh，或 responder 调度。
- 它是 server-side issuance path，不替代 client-side stapled-response verification 或 client online OCSP check。

### WolfSSL 上的对应路径（实验性）

`WolfSSL` 当前也已经接通了最小的 stapled-response public/runtime surface，但它仍然只能按实验性能力看待。

最短配置形态是：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslWolfSSL)
  .WithCertificate('server.crt')
  .WithPrivateKey('server.key')
  .WithOCSPStapling(True)
  .WithServerOCSPStapledResponseFile(
    'tests/fixtures/p2/ocsp/ocsp_response_successful_basic_v1.der')
  .BuildServer;
```

当前 `WolfSSL` 路径已经对齐的点：

- `WithOCSPStapling(True)` 会让 client connection 在握手前请求 `status_request`
- `WithServerOCSPStapledResponseFile(...)` 会把 caller-provided DER material 装到 server context，并注册 native status callback
- `ISSLOCSPStapling` 仍然是 client 读取 stapled response 的 surface
- `ISSLServerOCSPStaplingContext` 仍然是 server 侧 clear / set bytes / load file / has / get 的 public surface

当前仍然保留的边界是：

- 这条路径只保证 request / consume / manual issuance 接线，不代表已经有充分的 runtime 生产证据
- 它同样不负责 online fetch、refresh，或 responder 调度

如果你还需要基于证书 AIA 主动发 online OCSP 请求，看下面的 FreePascal client online path。

---

## 4. 在 FreePascal client 上启用 online OCSP check

如果你希望 FreePascal client 在握手后按 leaf 证书 AIA 自动发 OCSP POST，可以打开 `sslCertVerifyCheckOCSP`，并提供 HTTP hooks。

最短用法是：

```pascal
uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithVerifyPeer
    .WithHTTPHooks(nil, @HandlePost)
    .BuildClient;

  Ctx.SetCertVerifyFlags([sslCertVerifyCheckOCSP]);
end;
```

这条路径当前已经锁定的语义是：

- 只在 `verify-peer` 的 non-resumed full-handshake path 上运行。
- leaf 证书必须带 AIA responder URL。
- transport 通过 context HTTP hooks 提供；库本身不做网络实现。
- `good` 状态放行。
- `revoked`、`unknown`、AIA 缺失、transport 失败，或校验错误都会 fail-closed。

还需要明确两个 boundary：

- 这条路径仍然依赖现有 OpenSSL OCSP helper 与 context HTTP hooks，不是库内建的完整在线 revocation stack。
- FreePascal server-side stapling issuance 已经通过 public optional context interface + builder file config 收口；当前这条 client online path 仍然不负责把服务端 stapling sourcing 变成自动 online fetch / refresh。

如果你需要手工构造或发送 OCSP 请求，而不是走 connection verify path，用下面的 OpenSSL helper 工作流。

---

## 5. 需要主动构造/发送请求时，走 OpenSSL helper 工作流

`src/fafafa.ssl.openssl.api.ocsp.pas` 提供：

- 模块加载：`LoadOpenSSLOCSP`
- 请求构造：`CreateOCSPRequest`
- 请求发送：`SendOCSPRequest`
- 响应验证：`VerifyOCSPResponse`
- 一站式状态检查：`CheckCertificateStatus`

### 最小初始化

```pascal
uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ocsp;

begin
  LoadOpenSSLCore;

  if not LoadOpenSSLOCSP(GetCryptoLibHandle) then
    raise Exception.Create('加载 OCSP 模块失败');
end;
```

### 在线 OCSP 检查

> 注意：`fafafa.ssl` 不实现网络通信。
>
> - `SendOCSPRequest` 只负责把 OCSP request 编码为 DER，并通过 `fafafa.ssl.net.hooks` 调用上层提供的 HTTP POST。
> - 是否支持 `http/https`、DNS、socket、TLS 验证（证书/主机名）等，全部由你的 hooks 传输实现决定。

```pascal
uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.net.hooks,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.api.x509;

var
  Scope: TSSLHTTPHooksScope;
  Req: POCSP_REQUEST;
  Resp: POCSP_RESPONSE;
  Status: Integer;
  Cert, Issuer: PX509;
  TrustStore: PX509_STORE;
begin
  // 由上层注入 HTTP POST（示例：线程局部 hooks）
  // Scope := TSSLHTTPHooksScope.Push(TSSLHTTPHooks.Create(nil, @YourTransport.HTTPPost));
  // try

  Req := CreateOCSPRequest(Cert, Issuer);
  if Req = nil then
    raise Exception.Create('创建 OCSP 请求失败');

  try
    Resp := SendOCSPRequest(Req, 'http://ocsp.example.com', 10, TrustStore);
    if Resp = nil then
      raise Exception.Create('发送 OCSP 请求失败');

    try
      if not VerifyOCSPResponse(Resp, Cert, Issuer, TrustStore, Req) then
        raise Exception.Create('OCSP 响应验证失败');

      Status := CheckCertificateStatus(Cert, Issuer, 'http://ocsp.example.com', 10, TrustStore);
      WriteLn('OCSP 证书状态码: ', Status);
    finally
      OCSP_RESPONSE_free(Resp);
    end;
  finally
    OCSP_REQUEST_free(Req);
  end;

  // finally
  //   Scope.Pop;
  // end;
end;
```

说明：

- `SendOCSPRequest` 不实现 `http/https` 传输；HTTP 行为由你注入的 hooks 决定。
- `VerifyOCSPResponse` 使用 fail-closed 逻辑，响应状态异常或签名校验失败都会返回 `False`。

---

## 5. 离线 deterministic 验证

Q1 P2 建议把高价值失败路径放在离线测试中，以保证稳定复现。

已落地用例：

- malformed 响应：`tests/certificate/test_p2_ocsp_comprehensive.pas:264`
- truncated 请求：`tests/certificate/test_p2_ocsp_comprehensive.pas:308`
- 时间窗口失败：`tests/certificate/test_p2_ocsp_comprehensive.pas:334`
- 无签名响应失败：`tests/certificate/test_p2_ocsp_comprehensive.pas:392`

离线夹具：

- `tests/fixtures/p2/ocsp/ocsp_response_malformed_v1.der`

---

## 5. OpenSSL 1.1.1 vs 3.x 注意点

在 `test_p2_ocsp_comprehensive` 中，部分 1.x only 函数在 3.x 下按分支跳过：

- 响应操作分支：`tests/certificate/test_p2_ocsp_comprehensive.pas:125`
- 验证分支：`tests/certificate/test_p2_ocsp_comprehensive.pas:178`
- 工具函数分支：`tests/certificate/test_p2_ocsp_comprehensive.pas:237`

这类跳过不影响 OCSP 主流程验证：请求构造、响应解析，以及有效期/签名失败路径仍然被覆盖。

---

## 6. 回归命令

Run:

```bash
bash scripts/run_all_module_tests.sh --modules OCSP --verbose
```

Then:

```bash
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```

---

## 7. 相关文档

- `docs/test_reports/P2_OCSP_MODULE_REPORT.md`
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`
