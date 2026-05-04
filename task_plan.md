# Task Plan - Backend Broad Completion Audit

## Goal
把 broad objective `继续工作,直到 各个后端的 接口和实现都完整` 拆成实际 checklist，并重新拉 fresh evidence 去判断当前仓库到底是已经完成，还是只剩 `WinSSL` 的真实 Windows runtime proof 这一个外部 blocker。

## Current Batch
1. 建立 broad objective 的 deliverable checklist：
   - 公开接口合同
   - capability / `KnownIssues` truth
   - Linux 主机可验证实现 gate
   - `WinSSL` runtime proof 或 blocker evidence
2. 重新拉 fresh evidence：
   - `backend_contract`
   - `capability_cache`
   - `compile_all_modules.py`
   - `run_freepascal_tls13_completeness_gate.sh`
   - `WinSSL` 环境探针与 validation bundle contracts
3. 按 checklist 判定当前 broad objective 是否真的闭合；若未闭合，只保留真实 blocker，不再盲开新功能线。

## Status
- [completed] Checklist and fresh evidence audit
- [completed] Coverage decision against the broad objective
- [completed] Final blocker statement

## Current Evidence
- `command -v pwsh` 本批 fresh probe 为空；`wine --version` 在当前 Linux 主机 fresh probe 仍然退出 `159`。
- `tests/contract/test_backend_contract.pas` fresh result:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
  - `./tmp/backend_contract_units/test_backend_contract`
  - 结果：`Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - 覆盖解释：`OpenSSL` / `WolfSSL` / `MbedTLS` / `FreePascal` 的 `Contract 1-21` 全绿；`WinSSL` 全部因为平台 unavailable 跳过，`Contract 15` 还显式保留 `Windows-focused batch` 边界。
- `tests/test_capability_cache.pas` fresh result:
  - `fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas`
  - `./tmp/capability_cache_units/test_capability_cache`
  - 结果：
    - `FreePascal KnownIssues runtime alignment verified`
    - `WolfSSL KnownIssues runtime alignment verified`
    - `MbedTLS KnownIssues runtime alignment verified`
- Linux 主机实现 gate fresh result:
  - `python3 scripts/compile_all_modules.py` => `185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505`
    - summary: `tmp/test-reports/freepascal_tls13_completeness_broad_completion_audit_20260505.md`
    - 结果：`17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
    - 结果：compile `185/185`、模块测试 `17/17`、Phase2 baseline dry-run 可用、最终 `[PASS] minimal CI gate finished`
- WinSSL repo-side source / bundle truth fresh result:
  - `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh` => PASS
  - `bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh` => PASS
  - `bash tests/scripts/test_winssl_connection_context_access_contract.sh` => PASS
  - `bash tests/scripts/test_winssl_session_truth_source_contract.sh` => PASS
  - `bash tests/scripts/test_winssl_context_external_store_contract.sh` => PASS
- broad objective 的 fresh audit 结论：
  - 当前 Linux 主机可验证范围内，没有新的 repo-side drift
  - 唯一未闭合 requirement 是 `WinSSL` 的真实 Windows runtime proof

## Verification Plan
- 全部已完成，见 `Current Evidence`。

## Risks
- `backend_contract` 的全绿只能证明当前 Linux 主机可验证 backend 的 public surface；不能拿它替代 `WinSSL` 的真实 Windows runtime proof。
- `FreePascal` completeness gate 和 minimal CI gate 只说明 Linux 主机上的主实现线、模块线和相邻回归没有 fresh regression；它们不覆盖 Windows runtime。
- `WinSSL` 的 source contract / bundle contract 已绿，但这仍不等于真实 Windows 主机上的握手、证书存储、session resumption、server/client runtime 行为已证实。

## Follow-up Queue
1. 当前 broad objective 的唯一剩余 requirement：真实 Windows 主机上的 `WinSSL` runtime proof。
2. 需要按 `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 在 Windows 主机上依次拿到 quick smoke、minimal gate、Wave B Windows gate、broader suite 的产物。
3. 在拿到 Windows 环境之前，不再在当前 Linux 主机继续扩新的 backend 功能线。
