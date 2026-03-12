# 2026-03-10 Wave B CI gate dry-run examples observability

## Goal
- 补齐 `run_wave_b_ci_gate.sh --dry-run` 对 examples selection/warning 的可观测面。
- 让 dry-run 输出与实际 summary 在 Linux examples metadata 上保持一致。

## Scope
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_dryrun_examples_observability_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 确认 dry-run 缺口
- [x] 新增 focused shell contract
- [x] 最小补齐 dry-run 输出
- [x] 跑 focused + 相邻 CI gate 合同回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/run_wave_b_ci_gate.sh tests/scripts/test_wave_b_ci_gate_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_dryrun_examples_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_default_alias_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_default_none_contract.sh` => PASS

## Result
- `run_wave_b_ci_gate.sh --dry-run` 现在会显式输出：
  - `examples_selection`
  - `examples_warning`
- 这让 dry-run 观察面与真实 summary 在 Linux examples metadata 上对齐，不再只有 step 命令而看不到选择/警告状态。

## Next Queue
- 继续扫其它 Wave B producer 的 dry-run 输出，看看是否还有同类 metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
