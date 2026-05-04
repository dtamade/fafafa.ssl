# Task Plan - WolfSSL Feature Capability Runtime Consistency

## Goal
让 WolfSSL library 的 capability matrix 不再把 `SNI` / `ALPN` / `SessionTickets` 硬编码成可用，而是和真实 helper surface 对齐，收掉 `GetCapabilities` / `IsFeatureSupported` 的 runtime truth 漂移。

## Current Batch
1. 先补 focused RED：
   - 在 `tests/test_wolfssl_framework.pas` 里先手动 `LoadWolfSSLLibrary`
   - 临时清空 `wolfSSL_UseSNI`、`wolfSSL_UseALPN` / `wolfSSL_ALPN_GetProtocol`、`wolfSSL_get_session` / `wolfSSL_set_session`
   - 再让 `TWolfSSLLibrary.Initialize` 基于这组 helper-loss 状态做 capability 检测
   - 断言 `SupportsSNI` / `SNISupport`、`SupportsALPN` / `ALPNSupport`、`SupportsSessionTickets` / `SessionTicketsSupport` 都应收敛到 `False/None`
2. 然后做最小生产修复：
   - `src/fafafa.ssl.wolfssl.lib.pas` 的 `DetectCapabilities` 不再硬编码 `HasALPN` / `HasSessionTickets`
   - `GetCapabilities` 的 `SNISupport` / `ALPNSupport` / `SessionTicketsSupport` 基于这些检测结果发布 `stable` 或 `none`
3. 跑 focused framework test、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，再写回台账并提交。

## Status
- [completed] 计划与 RED 测试
- [completed] WolfSSL capability truth 修复
- [completed] Verification
- [completed] Review and commit

## Verification Summary
- focused framework contract:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/wolfssl_framework_units -FEtmp/wolfssl_framework_units -otmp/wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
  - `./tmp/wolfssl_framework_units/test_wolfssl_framework`
  - 结果：`Total: 110 / Passed: 110 / Failed: 0 / Rate: 100.0%`
- compile gate:
  - `python3 scripts/compile_all_modules.py`
  - 结果：`185/185` 核心模块编译成功
- minimal CI gate:
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - 结果：compile gate `185/185`、模块回归 `17/17`、phase2 dry-run 通过，最终 `[PASS] minimal CI gate finished`

## Risks
- 这批只修 capability truth，不新增任何新的 SNI/ALPN/session feature 实现。
- helper-loss RED 依赖 `LoadWolfSSLLibrary` 后的函数指针覆写；测试必须在 `finally` 中把库卸载回干净状态，不能污染后续 framework 场景。
- `SessionTicketsSupport` 在这里继续以现有 session get/set helper surface 为 truth source，不把这批扩大成完整 session resumption runtime 审计。

## Follow-up Queue
1. 如果这批收口完成，下一步继续看还有没有其它 backend 的 capability/runtime truth 分叉没有关完。
2. 更广的 backend completeness 仍要继续批次化推进，但每次只锁一组 capability/interface truth。
