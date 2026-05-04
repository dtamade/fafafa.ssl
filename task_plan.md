# Task Plan - Backend Context Optional Interface Completion Audit

## Goal
把 context-level `ISSLEarlyDataContext` / `ISSLServerOCSPStaplingContext` 的 public contract 完整锁住：既验证 unsupported backend 的 interface absent，也验证 capability 为 usable 的 backend 真的暴露对应接口。

## Current Batch
1. 在 `tests/contract/test_backend_contract.pas` 增加 context-level completion-audit contract，把 capability 和 `ISSLEarlyDataContext` / `ISSLServerOCSPStaplingContext` 的双向关系锁住。
2. 先跑 focused contract，判断当前剩余问题是“接口没挂上”还是“capability 写宽了”；只有出现真实 RED 才改生产代码。
3. 跑 `python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，把这批作为 completion audit 或最小修复批次收口提交。

## Status
- [completed] RED/audit contract
- [completed] GREEN implementation
- [completed] Verification
- [in_progress] Review and commit

## Outcome
- `Contract 12` 现已把 context/connection early-data surface 与 capability 做成双向约束。
- `WolfSSL` 在当前主机 `EarlyDataSupport=None` 时不再暴露 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`，同时保留 `OCSPStaplingSupport<>None` 时的 `ISSLServerOCSPStaplingContext`。
- 验证已完成：
  - `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract` => `Passed: 75 / Failed: 0 / Skipped: 15`
  - `python3 scripts/compile_all_modules.py` => `185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => `[PASS] minimal CI gate finished`

## Risks
- 当前批次已经收掉 Linux 主机可直接修的 `WolfSSL` early-data public drift；剩余 closeout 更偏向其它 optional/public surface 审计与平台 runtime 证据。
- `WinSSL` 仍无法在当前 Linux 主机做 runtime 证明，因此跨平台完成度不能只靠本批结论外推。

## Follow-up Queue
1. 继续审计其它尚未被 cross-backend contract 双向锁住的 optional/public surface。
2. 整理 closeout 剩余的“只能靠平台 runtime 证据确认”的项目，尤其是 Windows/WinSSL 边界。
