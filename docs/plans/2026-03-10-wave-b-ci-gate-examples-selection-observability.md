# 2026-03-10 Wave B CI gate examples selection observability

## Goal
- 补齐 `run_wave_b_ci_gate.sh` summary 对 examples selection 的可观测面。
- 让 producer summary 和 cross-summary / evidence / handoff 一样，显式写出 Linux examples selection 状态。

## Scope
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_examples_selection_explicit_override_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_examples_selection_default_alias_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 producer summary 缺失的 selection 面
- [x] 新增 explicit/default focused contracts
- [x] 最小补齐 summary 输出
- [x] 跑 focused + 相邻 CI gate 合同回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/run_wave_b_ci_gate.sh tests/scripts/test_wave_b_ci_gate_examples_selection_explicit_override_contract.sh tests/scripts/test_wave_b_ci_gate_examples_selection_default_alias_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_selection_default_alias_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_explicit_override_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_examples_warning_default_none_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_cli_reports_dir_passthrough_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_ci_gate_mixed_output_priority_contract.sh` => PASS

## Result
- `run_wave_b_ci_gate.sh` summary 现在会显式输出 examples selection：
  - explicit override -> `explicit_override`
  - default path -> `current_alias`
- 这样 producer summary 也具备了 selection + warning 双字段，与下游 consumer 的观测面更接近。

## Next Queue
- 继续扫 Wave B 其它 producer 是否还缺 selection/warning 这类可观测字段。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
