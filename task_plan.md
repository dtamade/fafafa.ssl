# Task Plan - WinSSL Windows Validation Bundle Truth Alignment

## Goal
把 WinSSL 的 Windows runtime validation bundle 收口到当前仓库真相：`tests/windows` 文档只引用真实存在的入口，手动 PowerShell 验证脚本不再依赖启动 cwd，并把剩余 blocker 明确压缩到“缺 Windows 主机实跑证据”。

## Current Batch
1. 先补 focused RED：
   - 新增 `tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
   - 锁住 `tests/windows/*.md` 不能再引用 `Run-WindowsValidation.ps1`、`Run-QuickValidation.ps1`、`test_cert_load`、`test_factory_mode` 等旧模板名称
   - 锁住 `tests/quick_winssl_validation.ps1` / `tests/run_winssl_tests.ps1` 必须自解析到 `tests/winssl`
2. 最小 GREEN：
   - `tests/quick_winssl_validation.ps1` 自动切到 `tests/winssl`
   - `tests/run_winssl_tests.ps1` 自动切到 `tests/winssl`
   - `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 改成当前真实验证顺序
   - `tests/windows/VALIDATION_BUNDLE.md` 改成当前真实 bundle inventory / artifact map
   - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 补 checklist / bundle 入口
3. 跑 focused contract / diff hygiene，回写台账并提交。

## Status
- [completed] RED: validation-bundle drift contract
- [completed] GREEN: script entrypoint truth alignment and docs rewrite
- [completed] Verification, review, and commit

## Verification Plan
- focused validation-bundle contract:
  - `bash -n tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
  - `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
- existing Wave B Windows gate contract:
  - `bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh`
- hygiene:
  - `git diff --check -- docs/plans/2026-05-05-winssl-windows-validation-bundle-truth-alignment.md tests/scripts/test_winssl_windows_validation_bundle_contract.sh tests/quick_winssl_validation.ps1 tests/run_winssl_tests.ps1 tests/windows/WINDOWS_VALIDATION_CHECKLIST.md tests/windows/VALIDATION_BUNDLE.md docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md task_plan.md findings.md progress.md`

## Risks
- 这批不能顺手改 WinSSL 生产实现；如果 Windows runtime proof 未来暴露真实行为缺口，必须另起实现批次。
- 文档必须区分三个层次：Linux source/compile proof、Windows runner/gate、Windows 主机 runtime proof；不能把它们混写成“已完整验证”。
- `tests/*.ps1` 只允许做 cwd 自解析和入口收口，不能把脚本扩张成新的 orchestration 框架。

## Batch Result
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 和 `tests/windows/VALIDATION_BUNDLE.md` 已从旧模板收口到当前真实入口:
  - `tests/quick_winssl_validation.ps1`
  - `run_winssl_tests.ps1`
  - `scripts/run_wave_b_windows_gate.ps1`
  - `tests/run_winssl_tests.ps1`
- `tests/run_winssl_tests.ps1` 现在不再重声明 common `-Verbose`，并且会自动切到 `tests/winssl`；`Backend Comparison Tests` 也已改回真实的 `tests/integration/test_backend_comparison.lpi`
- `tests/quick_winssl_validation.ps1` 现在也会自动切到 `tests/winssl`，不再要求调用者先手动 `cd`
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 已补当前 checklist / bundle 执行口径
- 新增 `tests/scripts/test_winssl_windows_validation_bundle_contract.sh`，并且与现有 `test_wave_b_windows_gate_pwsh_and_verbose_contract.sh` 一起通过
- 因此当前 broad objective 的 repo-side 剩余阻塞已进一步收紧到“等待真实 Windows 主机 runtime proof”

## Follow-up Queue
1. 这批完成后，下一步应在真实 Windows 主机按 checklist 跑 quick smoke、Wave B gate、broader suite，拿到 runtime 证据。
2. 只有 Windows 主机实跑出现 fresh RED 时，才重开 `src/fafafa.ssl.winssl.*` 实现修复。
3. 若 Windows host 不可用，broad objective 仍不能标记为“各个后端的接口和实现都完整”。
