# 2026-03-10 Wave B evidence dry-run path observability

## Goal
- 补齐 `check_wave_b_b2_evidence_consistency.sh --dry-run` 对关键输入报告路径的可观测面。
- 让 evidence dry-run 不只给判定结果，也显式给出用到的 summary / cross / closure 路径。

## Scope
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_paths_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 evidence dry-run path 缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run 输出
- [x] 跑 focused + 相邻 evidence dry-run/warning 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/check_wave_b_b2_evidence_consistency.sh tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_paths_contract.sh tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_paths_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_evidence_consistency_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_evidence_consistency_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_b2_evidence_consistency_examples_warning_default_none_contract.sh` => PASS

## Result
- evidence dry-run 现在会显式输出：
  - `linux_summary`
  - `macos_summary`
  - `windows_summary`
  - `cross_summary`
  - `closure_report`
- 这样 evidence dry-run 在输入路径层也更完整，不再只给 mismatch/selection/warning 结果。

## Next Queue
- 继续扫其它 Wave B producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
