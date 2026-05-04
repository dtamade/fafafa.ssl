# Task Plan - C-Library KnownIssues Truth Alignment

## Goal
把当前可在 Linux 主机验证的 C-library backend capability `KnownIssues` 收口到真实剩余边界：`WolfSSL` 明确改成 build/runtime helper-gated truth，`MbedTLS` 明确改成 early-data / OCSP stapling / CT unsupported truth。`WinSSL` 真实 Windows runtime proof 仍保留为外部 blocker，不在这批范围内。

## Current Batch
1. 先补 focused RED：
   - 在 `tests/test_capability_cache.pas` 增加 `WolfSSL` / `MbedTLS` `KnownIssues` truth assertions
   - 用当前 runtime capability truth 锁住：`WolfSSL` 不能继续只写 generic build-options wording，`MbedTLS` 不能继续只写 enterprise-features wording
2. 最小 GREEN：
   - `src/fafafa.ssl.wolfssl.lib.pas` 的 `KnownIssues` 收口到 helper-gated early-data / experimental OCSP truth
   - `src/fafafa.ssl.mbedtls.lib.pas` 的 `KnownIssues` 收口到 early-data / OCSP stapling / CT unsupported truth
3. 跑 focused capability test / diff hygiene，回写台账并提交。

## Status
- [completed] RED: c-library KnownIssues wording truth contract
- [completed] GREEN: WolfSSL / MbedTLS capability wording alignment
- [completed] Verification
- [completed] Review and commit

## Current Evidence
- `tests/contract/test_backend_contract.pas` 当前已经把 public interface 合同面收口到 `Contract 1-21`；Linux 主机 fresh evidence 为 `135 total / 111 passed / 0 failed / 24 skipped`，其中 `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 都是全绿，`WinSSL` 全部因为平台不可用而跳过。
- `command -v pwsh` 为空，`wine --version` 仍然在当前 Linux 主机退出 `159`，所以 `WinSSL` 真实 Windows runtime proof 依旧不能在本机完成。
- `tests/test_capability_cache.pas` 现在已覆盖 `OpenSSL` cache、`FreePascal` `KnownIssues`、以及 `WolfSSL` / `MbedTLS` runtime wording truth。
- `src/fafafa.ssl.wolfssl.lib.pas` 的 `KnownIssues` 现在明确为 build/runtime helper 门控；缺 helper 时 early-data 可能退化为 `none`，并保留 `OCSP stapling remains experimental`。
- `src/fafafa.ssl.mbedtls.lib.pas` 的 `KnownIssues` 现在明确为 early-data / OCSP stapling / CT 当前不支持，而不是继续保留 generic placeholder。
- fresh focused verification 已通过：
  - `fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otest_capability_cache tests/test_capability_cache.pas`
  - `./tmp/capability_cache_units/test_capability_cache`
- 最终 hygiene 已通过：
  - `git diff --check -- docs/plans/2026-05-05-c-library-knownissues-truth-alignment.md tests/test_capability_cache.pas src/fafafa.ssl.wolfssl.lib.pas src/fafafa.ssl.mbedtls.lib.pas task_plan.md findings.md progress.md`

## Verification Plan
- focused capability test:
  - `mkdir -p tmp/capability_cache_units`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otest_capability_cache tests/test_capability_cache.pas`
  - `./tmp/capability_cache_units/test_capability_cache`
- hygiene:
  - `git diff --check -- docs/plans/2026-05-05-c-library-knownissues-truth-alignment.md tests/test_capability_cache.pas src/fafafa.ssl.wolfssl.lib.pas src/fafafa.ssl.mbedtls.lib.pas task_plan.md findings.md progress.md`

## Risks
- 这批不能把 `WolfSSL` 当前 host 的 helper 缺失，误写成 backend family 永久不支持 early-data。
- `KnownIssues` 要表达真实 capability caveat，而不是把文档态或 CI/host availability 混进去。
- `WinSSL` 外部阻塞仍然存在，这批不能假装把 Windows runtime proof 做完。

## Follow-up Queue
1. 若这批后仍有 fresh RED，优先继续 capability truth / roadmap truth 相邻 drift，不盲开新功能线。
2. `WinSSL` broad blocker 仍然是 Windows 主机 runtime proof。
3. 若 `WolfSSL` / `MbedTLS` wording 收口后，下一步再决定是否还要做 roadmap closeout 或继续等待外部 Windows 证据。
