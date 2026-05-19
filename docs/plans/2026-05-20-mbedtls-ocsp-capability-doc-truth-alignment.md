# MbedTLS OCSP Capability Doc Truth Alignment

## Goal

收口 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
里关于 `OCSP` / `OCSP Stapling`
的活跃文档漂移，明确区分：

- `fafafa.ssl` 当前已发布的 backend capability
- 调用方在库外自行实现的 application-layer revocation workflow

避免专页继续把
“需要应用层实现”
误读成
“当前 backend 只差调用方式没写出来”。

## Scope

- 新增 focused shell contract，锁住当前 source / test / dedicated-doc truth
- 最小修正 `MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`
- 不重开 MbedTLS 新功能实现
- 不扩到 TLS 1.3 / Windows / custom I/O 等其他 MbedTLS 话题

## Architecture Truth

- `src/fafafa.ssl.mbedtls.lib.pas`
  当前明确发布：
  - `OCSPStaplingSupport = sslSupportNone`
  - `sslFeatOCSPStapling = False`
  - `KnownIssues` 把
    `early-data / OCSP stapling / CT`
    统一列为当前不支持
- 当前 MbedTLS source 没有
  `sslCertVerifyCheckOCSP`
  相关处理，
  也就是没有 shipped online-OCSP verification public/runtime path
- `tests/mbedtls/test_mbedtls_context_contract.pas`
  已冻结：
  - `ISSLServerOCSPStaplingContext` 不应暴露
- `tests/contract/test_backend_contract.pas`
  已冻结：
  - `OCSPStaplingSupport=None`
    的 backend
    不应暴露
    `ISSLOCSPStapling`
- `TSSLContextBuilder.BuildServer`
  当前已锁定：
  - 如果配置
    `server_ocsp_stapled_response_file`
    但 backend 不支持
    `ISSLServerOCSPStaplingContext`
    会 fail-fast

## Files

- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh`
- `docs/plans/2026-05-20-mbedtls-ocsp-capability-doc-truth-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认当前 dedicated doc 对这批 truth 先 RED
3. 最小修正 MbedTLS dedicated matrix：
   - `OCSP`
   - `OCSP Stapling`
   - 限制与注意事项
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh
bash tests/scripts/test_mbedtls_ocsp_capability_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- MbedTLS 专页不再把
  `OCSP`
  写成
  `⚠️ 部分 | 需手动实现`
- 专页会明确：
  - 当前 backend 不发布 online OCSP public capability
  - 当前 backend 不发布 stapled-response owner surfaces
  - 如需相关 revocation workflow，
    需要在
    `fafafa.ssl`
    已发布 surface 之外由应用层自行实现
