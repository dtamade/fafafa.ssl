# 2026-02-24 Wave B Windows Gate 接入 WinSSL Blocker Batch

## Goal

- 将 `scripts/run_windows_winssl_blocker_batch_draft.sh` 接入 `scripts/run_wave_b_windows_gate.ps1`，形成更高层单入口守门。
- 在报告中提供可审计的 skip 语义（可控、可观测）。

## Architecture / Scope

- Script:
  - `scripts/run_wave_b_windows_gate.ps1`
  - 新增参数：`-SkipWinsslBlockerBatch`
  - 默认行为：执行 WinSSL blocker batch
  - skip 行为：Step Matrix 显示 `SKIP/SKIPPED`，并不阻断 overall
- Contract:
  - `tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
  - 以静态合同校验 PowerShell 脚本接线（Linux 环境可执行）

## Files

- `docs/plans/2026-02-24-wave-b-windows-gate-winssl-blocker-batch-integration.md`
- `scripts/run_wave_b_windows_gate.ps1`
- `tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
2. GREEN:
   - 修改 `scripts/run_wave_b_windows_gate.ps1`，接入 blocker batch + skip 语义 + summary 行。
3. Regression:
   - `bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
   - `bash tests/scripts/test_windows_winssl_blocker_batch_draft_dryrun_contract.sh`
   - `bash tests/scripts/test_windows_winssl_blocker_batch_draft_failure_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round5.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round5.md`
   - `bash -n tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh scripts/run_windows_winssl_blocker_batch_draft.sh tests/scripts/test_windows_winssl_blocker_batch_draft_dryrun_contract.sh tests/scripts/test_windows_winssl_blocker_batch_draft_failure_contract.sh tests/scripts/test_wave_b_cross_platform_summary.sh`

## Expected Outputs

- Wave B Windows gate summary 中新增 `winssl_blocker_batch` 行，默认参与门禁判定。
- 使用 `-SkipWinsslBlockerBatch` 时，报告中可见 `SKIP/SKIPPED`，overall 判定不受该步阻断。
- docs strict round5 继续保持 active noise = 0、index dedup = 0。
