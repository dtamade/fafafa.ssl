# Task Plan - Backend HTTP Hooks Interface Completion Audit

## Goal
把 context-level `ISSLHttpHooksAccess` public contract 补成 cross-backend completion audit：当前真正依赖 context HTTP hooks 的后端必须显式暴露该接口，其余后端继续保持 absent。

## Current Batch
1. 在 `tests/contract/test_backend_contract.pas` 增加 context-level HTTP hooks completion-audit contract，锁住 `ISSLHttpHooksAccess` 的 presence/absence 和最小 callback round-trip。
2. 先跑 focused contract，判断当前剩余问题是“接口没挂上”还是 callback storage round-trip 漂移；只有出现真实 RED 才改生产代码。
3. 跑 `python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，把这批作为 completion audit 或最小修复批次收口提交。

## Status
- [completed] RED/audit contract
- [completed] GREEN implementation not needed
- [completed] Verification
- [completed] Review and commit

## Outcome
- `Contract 14` 已把 context-level `ISSLHttpHooksAccess` 锁进 cross-backend completion audit。
- 当前 Linux 主机上，`OpenSSL` / `FreePascal` 的 client/server context 都能稳定暴露 HTTP-hooks surface，callback setter/getter round-trip 正常；`WolfSSL` / `MbedTLS` 继续保持接口 absent；`WinSSL` 因平台不可用而显式 skip。
- 这批没有命中新的生产代码漂移，属于 completion audit 收口。
- 验证已完成：
  - `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract` => `Passed: 83 / Failed: 0 / Skipped: 17`
  - `python3 scripts/compile_all_modules.py` => `185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local` => `[PASS] minimal CI gate finished`

## Risks
- 这批全绿只说明 HTTP hooks 已和当前 direct-context truth 对齐，不代表整个 closeout 已结束。
- 这批不重开 online OCSP / CT 运行时逻辑，只锁 context-level public surface。

## Follow-up Queue
1. 继续盘点其它尚未被 cross-backend contract 锁住的 optional/public surface，优先审计 `src/fafafa.ssl.wolfssl.context.pas` 中仍公开保留的旧 `TWolfSSLConnection` 类型是否构成真实 public completeness drift。
2. 对只能靠平台 runtime 证明的 WinSSL 边界继续单列，不在 Linux 主机上做假完成结论。
