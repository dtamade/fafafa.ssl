# 2026-03-10 Wave B CI gate dry-run summary observability

## Goal
- 补齐 `run_wave_b_ci_gate.sh --dry-run` 对 `run_id` / `summary_out` 的可观测面。
- 让 CI gate dry-run 不再只靠最后一行 `summary:` stderr 提示，而是有稳定的结构化 dry-run 字段。

## Scope
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_dryrun_summary_observability_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 CI gate dry-run summary 缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run summary 输出
- [x] 跑 focused + 相邻 CI gate dry-run 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/run_wave_b_ci_gate.sh tests/scripts/test_wave_b_ci_gate_dryrun_summary_observability_contract.sh tests/scripts/test_wave_b_ci_gate_dryrun_examples_paths_contract.sh tests/scripts/test_wave_b_ci_gate_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_dryrun_summary_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_dryrun_examples_paths_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_default_alias_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_default_none_contract.sh` => PASS

## Result
- CI gate dry-run 现在会显式输出：
  - `run_id`
  - `summary_out`
- 这样 CI gate dry-run 在 summary 层也有稳定的结构化观测面，不再只依赖非结构化 stderr 行。 

## Next Queue
- 继续扫其它 Wave B producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
