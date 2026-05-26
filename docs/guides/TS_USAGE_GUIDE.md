# TS 使用指南（OpenSSL 后端）

本文说明 `fafafa.ssl` 在 OpenSSL 后端下的时间戳（TS）模块最小接入方式与离线验证建议。

---

## 1. 核心能力

`src/fafafa.ssl.openssl.api.ts.pas` 提供：

- 模块加载：`LoadTSFunctions`
- 请求构造：`CreateTimestampRequest`
- 响应验证：`VerifyTimestampResponse`
- 时间提取：`GetTimestampTime`

---

## 2. 最小初始化

```pascal
uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ts;

begin
  LoadOpenSSLCore;
  LoadTSFunctions;
end;
```

---

## 3. 基础工作流

```pascal
uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ts,
  fafafa.ssl.openssl.api.x509;

var
  Req: PTS_REQ;
  Resp: PTS_RESP;
  Store: PX509_STORE;
  Data: TBytes;
  Verified: Boolean;
  TsTime: TDateTime;
begin
  // 待签时间戳的数据
  Data := BytesOf('timestamp-payload');

  Req := CreateTimestampRequest(Data);
  if Req = nil then
    raise Exception.Create('创建 TS 请求失败');

  // 说明：示例只演示本地验证流程。实际场景需把 Req 发送给 TSA，获取真实 Resp。
  Resp := nil;

  Verified := VerifyTimestampResponse(Resp, Req, Store);
  if not Verified then
    WriteLn('TS 响应验证失败或响应为空（预期）');

  TsTime := GetTimestampTime(Resp);
  if TsTime > 0 then
    WriteLn('时间戳时间: ', DateTimeToStr(TsTime));

  if Assigned(TS_REQ_free) and (Req <> nil) then
    TS_REQ_free(Req);
end;
```

---

## 4. 离线 deterministic 验证（推荐）

已落地失败场景：
- malformed 响应：`tests/certificate/test_p2_ts_comprehensive.pas:238`
- truncated 响应：`tests/certificate/test_p2_ts_comprehensive.pas:297`
- rejection/无状态失败：`tests/certificate/test_p2_ts_comprehensive.pas:335`
- 空响应签名失败：`tests/certificate/test_p2_ts_comprehensive.pas:410`

离线夹具：
- `tests/fixtures/p2/ts/ts_response_malformed_v1.der`

---

## 5. OpenSSL 1.1.1 vs 3.x 注意点

- TS BIO 符号在 3.x 下按 `d2i_TS_REQ_bio` / `d2i_TS_RESP_bio` 绑定。
- 测试中对部分 1.x only TS API 做分支跳过：
  - `tests/certificate/test_p2_ts_comprehensive.pas:128`
  - `tests/certificate/test_p2_ts_comprehensive.pas:142`
  - `tests/certificate/test_p2_ts_comprehensive.pas:215`

---

## 6. 回归命令

```bash
bash scripts/run_all_module_tests.sh --modules TS --verbose
```

如需与 P2 主线同步验证：

```bash
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```

---

## 7. 相关文档

- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`
