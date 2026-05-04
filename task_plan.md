# Task Plan - WinSSL Windows Runtime Proof Handoff

## Goal
把 broad objective 的唯一剩余 requirement 切成一个可直接在真实 Windows 主机执行的批次，并把当前台账从旧的 Linux 侧 `-Twin64` 交叉编译结论切换到真实下一步：`WinSSL` 的 runtime proof。

## Current Batch
1. 复核当前 broad blocker 是否已经稳定收敛到 Windows runtime proof。
2. 把 Windows 主机上的执行顺序、必留产物、验收标准写成正式计划。
3. 明确当前 Linux 主机已没有新的 repo-side 收口项，不再虚构新的“继续”批次。

## Status
- [completed] Linux-side closure reconfirmed from existing evidence
- [completed] Formalize the Windows-host runtime validation batch
- [pending] Run quick smoke on a real Windows host
- [pending] Run WinSSL minimal gate on a real Windows host
- [pending] Run Wave B Windows gate and preserve artifacts
- [pending] Run broader WinSSL suite and annotate high-risk areas

## Current Evidence
- fresh broad completion audit 已证明：
  - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
  - `tests/test_capability_cache.pas`：`FreePascal` / `WolfSSL` / `MbedTLS` wording truth 全绿
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505`：`17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：PASS
  - `WinSSL` source contract / bundle contract：全部 PASS
- fresh Win64 cross-target compile 已补齐：
  - `tests/winssl/test_winssl_session_management.pas` 可成功交叉编译到 Win64
  - `tests/integration/test_backend_comparison.pas` 可成功交叉编译到 Win64
- 当前 Linux 主机环境边界已确认：
  - `command -v pwsh`：空 / exit `1`
  - `wine --version`：exit `159`
- 结论：
  - Linux 侧 public surface、capability truth、repo gates、source contract、Win64 compile proof 都已闭合
  - 唯一未闭合 requirement 是真实 Windows 主机上的 `WinSSL` runtime proof

## Windows Host Execution Order
1. `powershell -ExecutionPolicy Bypass -File .\tests\quick_winssl_validation.ps1`
2. `powershell -ExecutionPolicy Bypass -File .\run_winssl_tests.ps1 -RunId winssl_min_20260505 -OutputDir test-reports`
3. `powershell -ExecutionPolicy Bypass -File .\scripts\run_wave_b_windows_gate.ps1 -RunId wave_b_windows_20260505 -OutputDir test-reports`
4. `Start-Transcript -Path .\test-reports\winssl_runtime_suite_20260505.log`
5. `powershell -ExecutionPolicy Bypass -File .\tests\run_winssl_tests.ps1`
6. `Stop-Transcript`

## Acceptance Artifacts
- `test-reports/wave_b_windows_gate_summary_<run_id>.md`
- `test-reports/wave_b_windows_winssl_<run_id>.log`
- `test-reports/wave_b_windows_openssl_<run_id>.log`
- `test-reports/wave_b_windows_modules_<run_id>.log`
- `test-reports/validate_all_modules_report_<run_id>.md`
- `test-reports/winssl_runtime_suite_<run_id>.log` or equivalent transcript

## Risks
- Linux 侧交叉编译和 source contract 不能替代 Windows runtime proof。
- 只有 quick smoke 或只有 Wave B gate，不足以把 WinSSL 写成 runtime proof complete。
- Windows 主机上的失败必须先分流成环境问题、入口脚本问题、还是实现缺口；只有最后一种才重开生产代码批次。

## Follow-up Queue
1. 获取真实 Windows 主机或等价 CI 访问。
2. 按 `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 执行并保留产物。
3. 如果 Windows runtime 全绿，再回仓库更新台账并判断 broad objective 是否可标记完成。
