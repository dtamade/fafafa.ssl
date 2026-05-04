# Task Plan - Certificate And Store Native-Handle Completion Audit

## Goal
把 `ISSLCertificate` / `ISSLCertificateStore` 上尚未进入 cross-backend completion audit 的 `ISSLNativeHandleAccess` public truth 补齐：基于 C 库 / OS-native 的 backend 需要暴露真实句柄，纯 Pascal backend 继续保持 absent。

## Current Batch
1. 先补 focused contract：
   - 在 `tests/contract/test_backend_contract.pas` 新增 `Contract 16` 与 `Contract 17`
   - certificate 走 loaded fixture 探针
   - certificate-store 走公开 `CreateCertificateStore()` 返回对象的 native-handle 探针
2. 跑 isolated backend contract：
   - `mkdir -p tmp/backend_contract_units`
   - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
   - `./tmp/backend_contract_units/test_backend_contract`
3. 若 RED 命中真实漂移，只做最小生产修复：
   - 优先收口在对应 backend 的 certificate / certstore 实现
   - 不扩大到 diagnostics / session resumption / verify / connection info
4. 复跑 focused contract + 仓库级 gate，回写台账并提交。

## Status
- [completed] Contract 16/17 completion audit scaffolding
- [completed] Focused RED/GREEN for certificate/store native-handle truth
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
- focused RED:
  - 初次 `./tmp/backend_contract_units/test_backend_contract`
  - 结果：`115` 项里 `93` 过、`2` 失败、`20` 跳过
  - 失败点只剩：
    - `CertificateStoreNativeHandleInterfaceAligned [WolfSSL]`
    - `CertificateStoreNativeHandleInterfaceAligned [MbedTLS]`
  - 共同信号：`ISSLNativeHandleAccess.IsNativeHandleValid returned False`
- focused GREEN:
  - 同一条 isolated contract 复跑后
  - 结果：`115` 项里 `95` 过、`0` 失败、`20` 跳过
  - 新增的 `Contract 16` / `Contract 17` 全绿
- repo gates:
  - `python3 scripts/compile_all_modules.py`
  - 结果：`185/185`，`100.0%`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - 结果：compile gate `185/185`，模块测试 `17/17`，phase2 baseline dry-run 通过，最终 `[PASS] minimal CI gate finished`

## Risks
- certificate contract 不能复用空 wrapper 真值；`MbedTLS` / `WolfSSL` / `WinSSL` 的空 certificate 初始句柄允许为 `nil`。
- store contract 不能为了探针去写入 WinSSL 系统证书存储；应只检查公开创建路径的 native-handle truth。
- `TMbedTLSCertificateStore` / `TWolfSSLCertificateStore` 这批只补了公开创建路径的 native-handle validity；store 内容同步到 native store 的更深语义仍可单开后续批次审计。

## Follow-up Queue
1. 如果这批只暴露 constructor/native-handle 漂移，优先做最小修复并提交。
2. 如果 RED 继续暴露 store 内容未同步到 native store，再单开下一批做 certificate-store verify/load parity。
3. 更广的 `ISSLDiagnostics` / `ISSLSessionResumption` / `ISSLCertificateVerification` / `ISSLConnectionInfo` 仍保持后续批次化推进。
