# Task Plan - Session-Resumption Interface Completion Audit

## Goal
把连接级 `ISSLSessionResumption` public surface 纳入 cross-backend completion audit，确认各 backend 的公开 connection 都暴露该接口，并且会话 getter / reused 状态与 core `ISSLConnection` 保持最小自洽。

## Current Batch
1. 先补 focused contract：
   - 在 `tests/contract/test_backend_contract.pas` 新增 `Contract 20`
   - 只审计 `ISSLSessionResumption`
   - 用 `TMemoryStream` 创建公开 connection 对象，不依赖真实网络握手
2. 跑 isolated backend contract：
   - `mkdir -p tmp/backend_contract_units`
   - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
   - `./tmp/backend_contract_units/test_backend_contract`
3. 若 RED 命中真实漂移，只做最小生产修复：
   - 优先收口在 `TBaseSSLConnection` 或 backend `DoGetSession` / `DoIsSessionReused` / `DoSetSession`
   - 不扩大到 `ISSLCertificateVerification` 或真实 runtime session reuse proof
4. 复跑 focused contract + 仓库级 gate，回写台账并提交。

## Status
- [completed] Contract 20 session-resumption completion audit scaffolding
- [completed] Focused completion audit for session-resumption interface truth
- [completed] Repository verification
- [completed] Review and commit preparation for a pure completion-audit batch

## Verification Plan
- focused:
  - `mkdir -p tmp/backend_contract_units`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
  - `./tmp/backend_contract_units/test_backend_contract`
- repo gates:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Batch Result
- `Contract 20` 已在当前 Linux 可验证 backend 上全绿：`Total Tests: 130 / Passed: 107 / Failed: 0 / Skipped: 23`
- `python3 scripts/compile_all_modules.py` 结果为 `185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` 结果为 `[PASS]`
- 这批没有打出任何 production drift，因此只新增 completion-audit contract 和规划台账，不修改 backend 实现

## Risks
- `ISSLSessionResumption` 与 core `ISSLConnection` 的 session surface 高度重叠，contract 必须锁“optional interface truth”，而不是误写成完整的 runtime resume 证明。
- `OpenSSL` / `MbedTLS` / `WolfSSL` / `FreePascal` / `WinSSL` 的 `DoGetSession` 初始行为未必完全一样；focused contract 只能要求 core getter 与 optional getter 在同一对象上自洽。
- 这批不能顺手扩大到 `ISSLCertificateVerification` 或真实跨连接恢复成功率，否则 scope 会重新发散。

## Follow-up Queue
1. 继续审计 `ISSLCertificateVerification`。
2. 若 certificate-verification contract 暴露 backend drift，优先收口 `DoGetPeerCertificateChain` / `DoGetVerifyResult` / `DoGetVerifyResultString`。
3. 若后续 runtime 或 contract 再打出 session-resumption 漂移，再回到 `DoGetSession` / `DoIsSessionReused` 做定点修复。
