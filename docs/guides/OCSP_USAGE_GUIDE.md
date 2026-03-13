# OCSP 使用指南（OpenSSL 后端）

本文面向 `fafafa.ssl` 的 OpenSSL 后端，给出 OCSP 基础接入方式与离线验证建议。

---

## 1. 核心能力

`src/fafafa.ssl.openssl.api.ocsp.pas` 提供：

- 模块加载：`LoadOpenSSLOCSP`
- 请求构造：`CreateOCSPRequest`
- 请求发送：`SendOCSPRequest`
- 响应验证：`VerifyOCSPResponse`
- 一站式状态检查：`CheckCertificateStatus`

---

## 2. 最小初始化

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

---

## 3. 在线 OCSP 检查（工作流）

> 注意：`fafafa.ssl` **不实现网络通信**。
>
> - `SendOCSPRequest` 仅负责把 OCSP request 编码为 DER，并通过 `fafafa.ssl.net.hooks` 调用上层提供的 HTTP POST。
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
- `VerifyOCSPResponse` 使用 fail-closed 逻辑，响应状态异常/签名校验失败都会返回 `False`。

---

## 4. 离线 deterministic 验证（推荐）

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

这类跳过不影响 OCSP 主流程（请求构造、响应解析、有效期/签名失败路径）验证。

---

## 6. 回归命令

```bash
bash scripts/run_all_module_tests.sh --modules OCSP --verbose
```

如需与 P2 主线同步验证：

```bash
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```

---

## 7. 相关文档

- `docs/test_reports/P2_OCSP_MODULE_REPORT.md`
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`
