# Task Plan - Backend Context Native-Handle Completion Audit

## Goal
把 context-level `ISSLNativeHandleAccess` public contract 补成 cross-backend completion audit：基于 C 库的 backend context 必须显式暴露该接口，纯 Pascal backend 继续保持 absent。

## Current Batch
1. 在 `tests/contract/test_backend_contract.pas` 增加 context-level native-handle completion-audit contract，锁住 `ISSLNativeHandleAccess` 暴露、backend type 和最小句柄有效性。
2. 先跑 focused contract，判断当前剩余问题是“接口没挂上”还是 getter/validity 真值漂移；只有出现真实 RED 才改生产代码。
3. 跑 `python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，把这批作为 completion audit 或最小修复批次收口提交。

## Status
- [completed] RED/audit contract
- [completed] GREEN implementation not needed
- [completed] Verification
- [in_progress] Review and commit

## Outcome
- `Contract 13` 已把 context-level `ISSLNativeHandleAccess` 锁进 cross-backend contract。
- 当前 Linux 主机上，`OpenSSL` / `WolfSSL` / `MbedTLS` context 都能稳定暴露 native-handle surface；`FreePascal` context 继续保持 absent；`WinSSL` 因平台不可用而显式 skip。
- 这批没有命中新的生产代码漂移，属于 completion audit 收口。
- 验证已完成：
  - `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract` => `Passed: 79 / Failed: 0 / Skipped: 16`
  - `python3 scripts/compile_all_modules.py` => `185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => `[PASS] minimal CI gate finished`

## Risks
- 这批全绿只说明 context native-handle 已和迁移文档对齐，不代表整个 closeout 已结束。
- `WinSSL` 在当前 Linux 主机仍只能走 availability skip，真正的 Windows runtime 证据仍是独立边界。

## Follow-up Queue
1. 继续盘点其它尚未被 cross-backend contract 锁住的 optional/public surface，优先挑能在 Linux 主机上直接形成 RED/GREEN 的条目。
2. 对只能靠平台 runtime 证明的 WinSSL 边界继续单列，不在 Linux 主机上做假完成结论。
