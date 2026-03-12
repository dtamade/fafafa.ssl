# 2026-03-10 Wave B cross/evidence dry-run examples path observability

## Goal
- 补齐 `generate_wave_b_cross_platform_summary.sh --dry-run` 与 `check_wave_b_b2_evidence_consistency.sh --dry-run` 对 `linux_examples_json` 路径的可观测面。
- 让 dry-run 输出不只给 selection/warning，也显式给出当前选中的 examples artifact 路径。

## Scope
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_dryrun_examples_observability_contract.sh`
- `tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_examples_observability_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 dry-run path 可观测缺口
- [x] 新增 focused shell contracts
- [x] 最小补齐 dry-run 输出
- [x] 跑 focused + 相邻 warning 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/generate_wave_b_cross_platform_summary.sh scripts/check_wave_b_b2_evidence_consistency.sh tests/scripts/test_wave_b_cross_platform_summary_dryrun_examples_observability_contract.sh tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_cross_platform_summary_examples_warning_default_none_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_evidence_consistency_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_evidence_consistency_examples_warning_default_none_contract.sh` => PASS

## Result
- cross-summary dry-run 现在会显式输出 `linux_examples_json`。
- evidence dry-run 现在也会显式输出 `linux_examples_json`。
- 这样 dry-run 观察面终于能同时看见：path + selection + warning。

## Next Queue
- 继续扫 Wave B 其它 producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
