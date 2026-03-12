# 2026-03-10 Wave B CI gate dry-run examples path observability

## Goal
- 补齐 `run_wave_b_ci_gate.sh --dry-run` 对 examples path 面的可观测性。
- 让 dry-run 不只打印 selection/warning，也显式打印 report/current_alias/run_scoped/archive 路径。

## Scope
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_dryrun_examples_paths_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 CI gate dry-run path 缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run path 输出
- [x] 跑 focused + 相邻 selection/warning 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/run_wave_b_ci_gate.sh tests/scripts/test_wave_b_ci_gate_dryrun_examples_paths_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_dryrun_examples_paths_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_default_alias_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_default_none_contract.sh` => PASS

## Result
- CI gate dry-run 现在会显式输出：
  - `examples_report`
  - `examples_current_alias`
  - `examples_run_scoped`
  - `examples_archive`
  - 以及已有的 `examples_selection` / `examples_warning`
- 这样 producer dry-run 观察面也具备完整 path + selection + warning。

## Next Queue
- 继续扫其它 Wave B producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
