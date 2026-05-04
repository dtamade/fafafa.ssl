# Task Plan - Connection-Info Interface Completion Audit

## Goal
把连接级 `ISSLConnectionInfo` public surface 纳入 cross-backend completion audit，确认各 backend 的公开 connection 都暴露该接口，并且返回的连接信息与 direct getter 保持最小自洽。

## Current Batch
1. 先补 focused contract：
   - 在 `tests/contract/test_backend_contract.pas` 新增 `Contract 19`
   - 只审计 `ISSLConnectionInfo`
   - 用 `TMemoryStream` 创建公开 connection 对象，不依赖真实网络握手
2. 跑 isolated backend contract：
   - `mkdir -p tmp/backend_contract_units`
   - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
   - `./tmp/backend_contract_units/test_backend_contract`
3. 若 RED 命中真实漂移，只做最小生产修复：
   - 优先收口在 `TBaseSSLConnection` 或 override `GetConnectionInfo` / `GetStateString` 的 backend connection
   - 不扩大到 session resumption / certificate verification
4. 复跑 focused contract + 仓库级 gate，回写台账并提交。

## Status
- [completed] Contract 19 connection-info completion audit scaffolding
- [completed] Focused completion audit for connection-info interface truth
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
- `Contract 19` 已在当前 Linux 可验证 backend 上全绿：`Total Tests: 125 / Passed: 103 / Failed: 0 / Skipped: 22`
- `python3 scripts/compile_all_modules.py` 结果为 `185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` 结果为 `[PASS]`
- 这批没有打出任何 production drift，因此只新增 completion-audit contract 和规划台账，不修改 backend 实现

## Risks
- `ISSLConnectionInfo` 当前大多走共享基类实现，但 `OpenSSL` / `WinSSL` 覆盖了 `GetConnectionInfo` 或 `GetStateString`，若 contract 出现 RED，优先检查 override 漂移。
- 这批不能顺手扩大到 `ISSLSessionResumption` / `ISSLCertificateVerification`，否则 scope 会重新发散。

## Follow-up Queue
1. 继续审计 `ISSLSessionResumption`。
2. `ISSLCertificateVerification` 继续保留在后续独立批次推进。
3. 若后续 runtime 或 contract 再打出 connection-info 漂移，再回到 `OpenSSL` / `WinSSL` override 做定点修复。
