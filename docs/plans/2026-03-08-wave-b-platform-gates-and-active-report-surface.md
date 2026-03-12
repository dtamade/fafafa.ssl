# 2026-03-08 Wave B platform gates and active report surface

## Goal

收口当前剩余的 active `test-reports` 默认输出/活动报告面，优先处理：

- Wave B macOS gate
- Wave B Windows gate
- `archive_ci_artifacts_draft.sh` 对 Wave B / TLS13 活动产物的扫描面
- `continuous_test_monitor.sh` 的默认监控输出目录

这波目标仍然是“阻止新噪音 + 对齐活动读取面”，不是清理历史 `test-reports/` 存量。

## Scope

- 脚本：
  - `scripts/run_wave_b_macos_gate.sh`
  - `scripts/run_wave_b_windows_gate.ps1`
  - `scripts/archive_ci_artifacts_draft.sh`
  - `scripts/continuous_test_monitor.sh`
- 合同：
  - 新增 repo-hygiene 默认路径合同
  - 新增 macOS gate + archive dry-run runtime 合同
  - 接入 repo-hygiene batch / coverage contract
- 文档与 working memory：
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Architecture

### Wave B platform gates

- 平台门禁共享目录：`tmp/wave_b_reports`
- 环境变量：`FAFAFA_WAVE_B_REPORTS_DIR`
- 约束：
  - macOS gate 默认输出落到共享目录
  - Windows gate 默认输出也落到同一共享目录
  - downstream 的 cross-summary / closure / consistency 链继续直接消费这组 summary 文件

### Active archive surface

- Wave B 活动目录：`tmp/wave_b_reports`
- TLS13 signer gate 活动目录：`tmp/tls13_signer_gate_reports`
- 历史兼容目录：`test-reports`（仅保留 generic/legacy 扫描面，不再作为 Wave B/TLS13 活动默认面）
- 环境变量：
  - `FAFAFA_WAVE_B_REPORTS_DIR`
  - `FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR`
  - `FAFAFA_ARCHIVE_LEGACY_REPORTS_DIR`

### Continuous monitor

- 监控默认目录：`tmp/continuous_test_monitor_reports/monitor`
- 环境变量：`FAFAFA_CONTINUOUS_MONITOR_REPORTS_DIR`

## RED -> GREEN Plan

1. 新增 `tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`
2. 新增 `tests/scripts/test_wave_b_platform_archive_default_reports_runtime_contract.sh`
3. 运行新合同，确认 RED
4. 修补 Wave B 平台 gate 默认目录与 archive 活动扫描面
5. 运行 focused contracts，确认 GREEN
6. 接入 repo-hygiene batch / coverage contract
7. 更新月度汇总与 working memory

## Planned Commands

Run:

```bash
bash tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh
bash tests/scripts/test_wave_b_platform_archive_default_reports_runtime_contract.sh
```

Then:

```bash
bash tests/scripts/test_wave_b_macos_gate_isolation_passthrough_contract.sh
bash tests/scripts/test_wave_b_macos_gate_fpc_host_passthrough_contract.sh
bash tests/scripts/test_wave_b_windows_gate_powershell_host_fallback_contract.sh
bash tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh
bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh
bash tests/scripts/test_continuous_test_monitor_isolation_passthrough_contract.sh
bash tests/scripts/test_archive_ci_artifacts_output_root_contract.sh
bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh
bash tests/scripts/test_repo_hygiene_contract_batch.sh
```

## Expected Outputs

- `run_wave_b_macos_gate.sh` / `run_wave_b_windows_gate.ps1` 默认输出统一到 `tmp/wave_b_reports`
- archive 脚本对 Wave B / TLS13 活动产物扫描统一到当前共享目录，而不是旧 `test-reports/`
- `continuous_test_monitor.sh` 默认监控输出切到 `tmp/continuous_test_monitor_reports`
- repo-hygiene batch 增加本波合同，防止默认路径回退
