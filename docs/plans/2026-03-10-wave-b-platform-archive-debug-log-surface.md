# 2026-03-10 Wave B platform archive debug log surface

## Goal
- 让 `archive_ci_artifacts_draft.sh` 把当前活动的 Wave B Linux/macOS/Windows gate 日志面纳入归档 dry-run / archive surface。
- 保持 Wave B summary 仍归类在 `core-reports`，新增日志只进入 `debug-logs` 类。

## Architecture
- 这波不改 archive 的 manifest 格式、retention policy、或 Wave B/TLS13 summary 归类。
- 只把当前活动日志 pattern 补到 `debug-logs`：
  - `wave_b_compile_*.log`
  - `wave_b_modules_*.log`
  - `wave_b_examples_*.log`
  - `wave_b_macos_*.log`
  - `wave_b_windows_*.log`
- 同步更新 repo-hygiene 静态合同，让 archive 对 Wave B/TLS13 env passthrough 和日志 pattern 都有稳定约束。

## Files
- `scripts/archive_ci_artifacts_draft.sh`
- `tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh`
- `tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Commands
1. `bash tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh`
2. `bash -n tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh tests/scripts/test_wave_b_platform_archive_default_reports_runtime_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`
3. `bash tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh`
4. `bash tests/scripts/test_wave_b_platform_archive_default_reports_runtime_contract.sh`
5. `bash tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`
6. `git diff --check -- scripts/archive_ci_artifacts_draft.sh tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`

## Expected Outputs
- RED：第 1 步先失败，说明 archive dry-run 还没扫描到 Wave B 活动日志。
- GREEN：archive dry-run 会显式列出 Linux/macOS/Windows gate 日志路径。
- Verification：新 runtime 合同、既有默认输出合同、repo-hygiene 静态合同都通过。

## Verification
- `bash tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh` => PASS
- `bash -n tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh tests/scripts/test_wave_b_platform_archive_default_reports_runtime_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_platform_archive_default_reports_runtime_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh` => PASS
- `git diff --check -- scripts/archive_ci_artifacts_draft.sh tests/scripts/test_wave_b_platform_archive_debug_logs_runtime_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh` => PASS

## Result
- archive dry-run / archive surface 现在也会收口当前活动的 Wave B debug logs。
- Wave B summary / evidence 与 debug logs 的归档层级更一致，后续排查不再只剩 markdown summary。
