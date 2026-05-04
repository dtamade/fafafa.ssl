# Task Plan - Diagnostics Interface Completion Audit

## Goal
把连接级 `ISSLDiagnostics` public surface 纳入 cross-backend completion audit，确认五个 backend 的公开 connection 都暴露同一组诊断接口，并保持最基本的字段自洽。

## Current Batch
1. 先补 focused contract：
   - 在 `tests/contract/test_backend_contract.pas` 新增 `Contract 18`
   - 只审计 `ISSLDiagnostics`
   - 用 `TMemoryStream` 创建公开 connection 对象，不依赖真实网络握手
2. 跑 isolated backend contract：
   - `mkdir -p tmp/backend_contract_units`
   - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
   - `./tmp/backend_contract_units/test_backend_contract`
3. 若 RED 命中真实漂移，只做最小生产修复：
   - 优先收口在 `TBaseSSLConnection` 或对应 backend connection 实现
   - 不扩大到 session resumption / certificate verification / connection info
4. 复跑 focused contract + 仓库级 gate，回写台账并提交。

## Status
- [completed] Contract 18 diagnostics completion audit scaffolding
- [completed] Focused RED/GREEN for diagnostics interface truth
- [completed] Repository verification
- [completed] Review and commit preparation

## Verification Plan
- focused:
  - `mkdir -p tmp/backend_contract_units`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
  - `./tmp/backend_contract_units/test_backend_contract`
- repo gates:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Verification Summary
- focused:
  - `./tmp/backend_contract_units/test_backend_contract`
  - 结果：`Total Tests: 120 / Passed: 99 / Failed: 0 / Skipped: 21`
  - 新增的 `Contract 18: Diagnostics interface alignment` 在所有当前可用 backend 上直接全绿
- repo gates:
  - `python3 scripts/compile_all_modules.py`
  - 结果：`185/185`，`100.0%`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - 结果：compile gate `185/185`，模块测试 `17/17`，phase2 baseline dry-run 通过，最终 `[PASS] minimal CI gate finished`

## Verification Plan
- focused:
  - `mkdir -p tmp/backend_contract_units`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
  - `./tmp/backend_contract_units/test_backend_contract`
- repo gates:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`

## Risks
- `ISSLDiagnostics` 当前由 `TBaseSSLConnection` 统一实现，若 contract 出现 RED，很可能意味着某个 backend 没有真正回到共享 connection 语义。
- 这批不能顺手扩大到 `ISSLSessionResumption` / `ISSLCertificateVerification` / `ISSLConnectionInfo`，否则 scope 会重新发散。
- 这批是 completion audit，没有新增生产修复；真正还未审计的 public surface 继续留在后续批次推进。

## Follow-up Queue
1. 如果 `ISSLDiagnostics` 直接全绿，就继续审计 `ISSLConnectionInfo`。
2. 如果 diagnostics contract 暴露 shared-base drift，优先收口共享实现，再看是否影响 `ISSLConnectionInfo`。
3. `ISSLSessionResumption` / `ISSLCertificateVerification` 继续保留在后续独立批次推进。
