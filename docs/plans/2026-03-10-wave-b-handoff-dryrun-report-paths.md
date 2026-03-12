# 2026-03-10 Wave B handoff dry-run report paths

## Goal
- 补齐 `prepare_wave_b_b2_handoff_bundle.sh --dry-run` 对 4 份目标报告路径的可观测面。
- 让 handoff dry-run 不只显示 `output_dir`，还显式显示 cross/closure/consistency/bundle 的最终输出路径。

## Scope
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_report_paths_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 handoff dry-run report-path 缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run 输出
- [x] 跑 focused + 相邻 handoff 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_report_paths_contract.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_report_paths_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_cli_reports_dir_passthrough_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_selection_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_warning_default_none_contract.sh` => PASS

## Result
- handoff dry-run 现在会显式输出：
  - `cross_summary`
  - `closure_report`
  - `consistency_report`
  - `bundle_report`
- 这样 handoff dry-run 在输出层也更完整，不再只给一个 `output_dir` 让调用者自己拼路径。

## Next Queue
- 继续扫其它 Wave B producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
