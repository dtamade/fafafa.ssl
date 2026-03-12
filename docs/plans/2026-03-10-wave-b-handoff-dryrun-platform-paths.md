# 2026-03-10 Wave B handoff dry-run platform paths

## Goal
- 补齐 `prepare_wave_b_b2_handoff_bundle.sh --dry-run` 对 macOS / Windows summary 输入路径的可观测面。
- 让 handoff dry-run 在平台输入层不再只剩 `macos_args/windows_args` 这种间接表达。

## Scope
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_platform_paths_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 handoff dry-run 平台路径缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run 平台路径
- [x] 跑 focused + 相邻 handoff dry-run/report 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_platform_paths_contract.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_report_paths_contract.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_platform_paths_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_report_paths_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_cli_reports_dir_passthrough_contract.sh` => PASS

## Result
- handoff dry-run 现在会显式输出：
  - `macos_summary`
  - `windows_summary`
- 这样 handoff dry-run 的平台输入路径层也更完整，不再只靠 `macos_args/windows_args` 让调用者自己反推。

## Next Queue
- 继续扫其它 Wave B producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
