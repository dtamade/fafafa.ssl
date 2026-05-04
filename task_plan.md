# Task Plan - Completion Audit And Capability Truth Alignment

## Goal
对“各个后端的接口和实现都完整”做一次真实 completion audit，把当前已经锁住的 interface surface、仍未完成的 implementation caveat、以及文档里高估能力等级的条目区分清楚；本批先收口 `docs/BACKEND_CAPABILITY_MATRIX.md` 与代码/测试/路线图之间的 FreePascal capability truth drift。

## Current Batch
1. 先做 completion audit：
   - 盘点 `src/fafafa.ssl.base.pas` 的 public interface 与 `tests/contract/test_backend_contract.pas` 的当前覆盖
   - 核对 `docs/ROADMAP.md`、`tests/test_capability_cache.pas`、`src/fafafa.ssl.freepascal.lib.pas`
   - 找出“接口已锁住，但实现/能力等级仍有限制”的真实剩余项
2. 只收口当前最明确的 truth drift：
   - 修改 `docs/BACKEND_CAPABILITY_MATRIX.md`
   - 让 FreePascal `Early Data` / `OCSP Stapling` / `Certificate Transparency` 的文案与 runtime capability truth 一致
3. 用 focused capability test 验证文档对应的真实代码发布值：
   - `mkdir -p tmp/capability_cache_units`
   - `fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas`
   - `./tmp/capability_cache_units/test_capability_cache`
4. 做 diff/format hygiene，回写台账并提交。

## Status
- [completed] Completion audit against current public surface and roadmap truth
- [completed] Capability-matrix truth alignment
- [completed] Focused capability verification
- [in_progress] Review and commit preparation

## Verification Plan
- audit evidence:
  - `docs/ROADMAP.md`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `tests/test_capability_cache.pas`
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
- focused:
  - `mkdir -p tmp/capability_cache_units`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas`
  - `./tmp/capability_cache_units/test_capability_cache`
- hygiene:
  - `yarn prettier --write docs/BACKEND_CAPABILITY_MATRIX.md`
  - `git diff --check`

## Batch Result
- completion audit 结论：当前这轮 connection optional interface completion audit 已收尽，但 broad objective 仍未完成
- 当前仍可证实的 implementation-level remaining gaps：
  - `FreePascal` `0-RTT / early data` 仍是 `experimental`，默认 shipped path 仍局限于单进程内存 anti-replay ledger
  - `WinSSL` 仍缺 Windows 主机上的 runtime proof；当前 Linux 侧只有 source-contract + Win64 cross-target compile evidence
- focused capability truth 证据：
  - `tests/test_capability_cache.pas` 运行通过，并直接验证 `KnownIssues = 0-RTT / early data is experimental...`
  - 同一 focused test 也验证 `ZeroRTTSupport` / `EarlyDataSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` 的 runtime truth
- 文档收口结果：
  - `docs/BACKEND_CAPABILITY_MATRIX.md` 不再把 FreePascal `Early Data` 写成“完整支持（生产就绪）”
  - FreePascal `OCSP Stapling` / `Certificate Transparency` 也不再在快速参考表里写成 `✅`

## Risks
- completion audit 不能把“contract 全绿”误当成“整体目标完成”；仍需单独识别 implementation caveat 与 runtime-proof 缺口。
- 文档收口这批不能假装解决了 `FreePascal` early-data experimental caveat，也不能伪造 `WinSSL` 的 Windows runtime proof。
- 只改最明确的 truth drift，不顺手重写整份能力矩阵。

## Follow-up Queue
1. 如果 completion audit 结论仍是 `FreePascal early-data experimental + WinSSL runtime proof missing`，下一步要在这两个 implementation gap 之间重新排优先级。
2. 若需要继续在 Linux 主机推进实现层，就优先考虑 `FreePascal` early-data 默认 shipped path caveat。
3. `WinSSL` 的 Windows runtime proof 仍需独立环境，不能在当前 Linux 主机上伪造完成。
