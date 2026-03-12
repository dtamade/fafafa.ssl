# 2026-03-10 Wave B handoff dry-run examples observability

## Goal
- 补齐 `prepare_wave_b_b2_handoff_bundle.sh --dry-run` 对 Linux examples path/selection/warning 的可观测面。
- 让 handoff dry-run 与其它 Wave B producer/consumer 的 dry-run 字段名保持一致。

## Scope
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_examples_observability_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 handoff dry-run 字段名缺口
- [x] 新增 focused shell contract
- [x] 最小统一 dry-run 字段名
- [x] 跑 focused + 相邻 handoff 合同回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_cli_reports_dir_passthrough_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_selection_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_examples_warning_default_none_contract.sh` => PASS

## Result
- handoff dry-run 现在也会显式输出：
  - `linux_examples_json`
  - `linux_examples_selection`
  - `linux_examples_warning`
- 其中 path 字段名已与 cross-summary / evidence dry-run 对齐，不再单独叫 `linux_examples`。

## Next Queue
- 继续扫 Wave B 其它 producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
