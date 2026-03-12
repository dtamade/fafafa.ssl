# 2026-03-11 pure Pascal SHA384 suite support

## Goal
- 为 pure Pascal / FreePascal backend 补齐 `TLS_AES_256_GCM_SHA384` 的真实握手支持。
- 这波至少覆盖：
  - Finished verify_data / finished key 的 SHA384 路径
  - server `CertificateVerify` transcript-input 的 SHA384 路径
  - client/server 握手面真实可协商到 `TLS_AES_256_GCM_SHA384`
  - capability truth 不再继续把 AES256-GCM-SHA384 标成 unsupported

## Root Cause
- `keyschedule` / `appschedule` / `AEAD` 已有 SHA384 分支。
- 但连接层仍硬编码在 SHA256：
  - `TLS13FinishedKeySHA256`
  - `TLS13VerifyFinishedSHA256`
  - `BuildTLS13ServerCertificateVerifyInputSHA256`
- 同时 pure Pascal clienthello / accept path 也没有把 `TLS_AES_256_GCM_SHA384` 真正纳入可协商交集。

## Files
- `src/fafafa.ssl.tls13.finished.pas`
- `src/fafafa.ssl.tls13.servercertverify.pas`
- `src/fafafa.ssl.tls13.clienthello.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_tls13_finished.pas`
- `tests/test_freepascal_local_sha384_suite_roundtrip.pas`
- `tests/test_freepascal_backend_basic.pas`
- `tests/test_capability_cache.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Plan
1. 红测：
   - SHA384 Finished 向量测试
   - local client/server roundtrip 协商 `TLS_AES_256_GCM_SHA384`
   - capability truth 改为支持 AES256GCM
2. 绿化：
   - 补 Finished / CertificateVerify SHA384 路径
   - 让 client/server 真正把 AES256 suite 纳入协商
   - 更新 capability truth
3. 回归：
   - roundtrip / stream / shutdown / compile
