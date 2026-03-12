# 2026-03-10 Wave B cross-summary dry-run platform paths

## Goal
- 补齐 `generate_wave_b_cross_platform_summary.sh --dry-run` 对 macOS/Windows/Android 输入路径的可观测面。
- 让 cross-summary dry-run 在平台输入层不再只给 state/note，也显式给出 probe/summary 路径。

## Scope
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_dryrun_platform_paths_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 cross dry-run 平台路径缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run 平台路径
- [x] 跑 focused + 相邻 cross-summary dry-run/warning 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/generate_wave_b_cross_platform_summary.sh tests/scripts/test_wave_b_cross_platform_summary_dryrun_platform_paths_contract.sh tests/scripts/test_wave_b_cross_platform_summary_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_dryrun_platform_paths_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_warning_default_none_contract.sh` => PASS

## Result
- cross-summary dry-run 现在会显式输出：
  - `macos_probe`
  - `macos_summary`
  - `windows_summary`
  - `android_summary`
- 这样 cross-summary dry-run 在平台输入路径层也更完整。

## Next Queue
- 继续扫其它 Wave B producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
