# MbedTLS TLS 1.3 Capability Doc Truth Alignment

## Goal

收口 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
里把 `TLS 1.3`
写成无条件 `✅ 支持`
的活跃文档漂移，
让专页重新回到当前 source / canonical matrix 已经表达的
“条件 capability truth”。

避免 dedicated MbedTLS page
继续把
“运行时版本条件成立时才发布”
误写成
“当前 backend 永远无条件支持”。

## Scope

- 新增 focused shell contract，锁住当前 source / canonical / dedicated-doc truth
- 最小修正 `MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`
- 不重开 MbedTLS 新实现
- 不扩到 OCSP / DTLS / custom I/O / Windows 等其他话题

## Architecture Truth

- `src/fafafa.ssl.mbedtls.base.pas`
  当前定义：
  - `MBEDTLS_MIN_VERSION = $03000000`
  - 注释直接说明：
    `3.0.0 minimum for TLS 1.3`
- `src/fafafa.ssl.mbedtls.lib.pas`
  当前明确发布：
  - `FCapabilities.HasTLS13 := FCapabilities.VersionNumber >= MBEDTLS_MIN_VERSION`
  - `IsProtocolSupported(sslProtocolTLS13) := FCapabilities.HasTLS13`
  - `Result.SupportsTLS13 := FCapabilities.HasTLS13`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
  顶层 quick reference
  当前已经把
  `MbedTLS`
  的
  `TLS 1.3`
  记为
  `⚠️`
- 但 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  当前仍写：
  - `TLS 1.3 | ✅ 支持 | MbedTLS 3.x 支持`

## Files

- `src/fafafa.ssl.mbedtls.base.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_mbedtls_tls13_capability_doc_truth_contract.sh`
- `docs/plans/2026-05-20-mbedtls-tls13-capability-doc-truth-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认当前 dedicated doc 对这批 truth 先 RED
3. 最小修正 MbedTLS dedicated matrix：
   - `TLS 1.3`
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_mbedtls_tls13_capability_doc_truth_contract.sh
bash tests/scripts/test_mbedtls_tls13_capability_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- MbedTLS 专页不再把
  `TLS 1.3`
  写成无条件 `✅ 支持`
- 专页会明确：
  - 当前 `SupportsTLS13` / `sslProtocolTLS13`
    取决于运行时版本检测
  - 只有检测到
    `MbedTLS 3.x+`
    时才发布 TLS 1.3 capability
  - dedicated page 与 canonical matrix
    不再分叉
