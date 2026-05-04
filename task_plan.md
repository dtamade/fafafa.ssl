# Task Plan - Backend Session Native-Handle Completion Audit

## Goal
把 session-level `ISSLNativeHandleAccess` public contract 补成有证据的 completion audit：C-library backend 的 session wrapper 必须稳定暴露 native-handle surface，纯 Pascal backend 继续保持 absent，同时不把 WinSSL 当前未厘清的 session truth split 混进这一批。

## Current Batch
1. 先给 `tests/contract/test_backend_contract.pas` 增加 `Contract 15`，用最小 session probe 锁住 `OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 的 session native-handle truth。
2. probe 只验证 public surface，不强行依赖完整握手：
   - `OpenSSL` 用 `SSL_SESSION_new`
   - `WolfSSL` / `MbedTLS` 用最小 wrapped opaque-handle probe
   - `FreePascal` 继续要求接口 absent
   - `WinSSL` 明确 skip，留给后续 Windows/session truth-source 专批
3. 如果 focused contract 直接全绿，这批作为 completion audit 收口；只有出现真实 RED 才进入最小修复。
4. 跑 `fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract`、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后更新台账并提交。

## Status
- [completed] 现状重载与目标收敛
- [completed] RED/audit contract
- [completed] GREEN implementation（无生产改动，作为 completion audit 收口）
- [completed] Verification
- [completed] Review and commit

## Risks
- `session` 级 native-handle 若只靠类声明判断，价值太低；因此 contract 需要至少验证 helper round-trip，而不是只看 `Supports(...)`。
- `WolfSSL` / `MbedTLS` 当前缺少廉价、稳定的独立 session allocator；这批只能做 wrapped-surface audit，不把它包装成完整 session resumption runtime proof。
- `WinSSL` 已发现 duplicate session truth source，这批不应在 Linux 主机上做过度结论。

## Follow-up Queue
1. 如果 `Contract 15` 全绿，下一批优先考虑 WinSSL session truth split 的 source-contract / Windows-bound 审计。
2. 如果 `Contract 15` 只在单个 backend 上出 RED，按最小边界补接口或 helper，而不是顺手重做整条 session/resumption 设计。
