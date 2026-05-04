# Task Plan - Certificate-Verification Interface Completion Audit

## Goal
把连接级 `ISSLCertificateVerification` public surface 纳入 cross-backend completion audit，确认各 backend 的公开 connection 都暴露该接口，并且证书链 / verify getter 与 core `ISSLConnection` 保持最小自洽。

## Current Batch
1. 先补 focused contract：
   - 在 `tests/contract/test_backend_contract.pas` 新增 `Contract 21`
   - 只审计 `ISSLCertificateVerification`
   - 用 `TMemoryStream` 创建公开 connection 对象，不依赖真实网络握手
2. 跑 isolated backend contract：
   - `mkdir -p tmp/backend_contract_units`
   - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
   - `./tmp/backend_contract_units/test_backend_contract`
3. 若 RED 命中真实漂移，只做最小生产修复：
   - 优先收口在 `TBaseSSLConnection` 或 backend `DoGetPeerCertificateChain` / `DoGetVerifyResult` / `DoGetVerifyResultString`
   - 不扩大到真实 runtime certificate validation parity 或 chain-building 重构
4. 复跑 focused contract + 仓库级 gate，回写台账并提交。

## Status
- [completed] Contract 21 certificate-verification completion audit scaffolding
- [completed] Focused completion audit for certificate-verification interface truth
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
- `Contract 21` 已在当前 Linux 可验证 backend 上全绿：`Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
- `python3 scripts/compile_all_modules.py` 结果为 `185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` 结果为 `[PASS]`
- 这批没有打出任何 production drift，因此只新增 completion-audit contract 和规划台账，不修改 backend 实现

## Risks
- `ISSLCertificateVerification` 与 core `ISSLConnection` 的 verify surface 高度重叠，contract 必须锁“optional interface truth”，而不是误写成完整的 runtime trust/hostname/revocation 证明。
- 各 backend 在未握手状态下的 `GetPeerCertificateChain` / `GetVerifyResultString` 默认值未必完全一样；focused contract 只能要求 core getter 与 optional getter 在同一对象上自洽。
- 这批不能顺手扩大到 runtime certificate validation parity、OCSP/CRL、CT 或 hostname 语义，否则 scope 会重新发散。

## Follow-up Queue
1. 回到剩余 public surface 总盘点，确认当前 completion audit 队列是否已收尽。
2. 若后续 runtime 或 contract 再打出 certificate-verification drift，优先收口 `DoGetPeerCertificateChain` / `DoGetVerifyResult` / `DoGetVerifyResultString`。
3. 若后续 runtime 或 contract 再打出 session-resumption 漂移，再回到 `DoGetSession` / `DoIsSessionReused` 做定点修复。
