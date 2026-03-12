# 2026-03-10 Wave B cross-summary examples warning observability

## Goal
- 补齐 `generate_wave_b_cross_platform_summary.sh` 对 `linux_examples_warning` 的可观测面。
- 让 cross-summary 与 evidence / handoff 在 Linux examples 选择链上继续保持同一组字段。

## Scope
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_examples_warning_explicit_override_contract.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_examples_warning_default_none_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 cross-summary 缺失的 warning 面
- [x] 新增 explicit/default 两条 focused contract
- [x] 最小补齐脚本输出
- [x] 跑 focused + 相邻 selection 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/generate_wave_b_cross_platform_summary.sh tests/scripts/test_wave_b_cross_platform_summary_examples_warning_explicit_override_contract.sh tests/scripts/test_wave_b_cross_platform_summary_examples_warning_default_none_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_warning_default_none_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_selection_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_selection_default_run_scoped_contract.sh` => PASS

## Result
- cross-summary 现在会显式输出：
  - `linux_examples_selection`
  - `linux_examples_warning`
- 这样 `producer -> cross-summary -> evidence -> handoff` 在 Linux examples 这条链上不再只有 selection 对齐，warning 面也一起对齐。

## Next Queue
- 继续扫同类 linked-evidence 边界，优先看 Wave B 其它 producer 是否还缺 `warning` / `selection` 这类可观测字段。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
