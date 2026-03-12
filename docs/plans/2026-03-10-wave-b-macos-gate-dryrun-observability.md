# 2026-03-10 Wave B macOS gate dry-run observability

## Goal
- 补齐 `run_wave_b_macos_gate.sh --dry-run` 的结构化 metadata 输出。
- 让 macOS gate dry-run 不只打印 step commands，还显式输出 run_id / output_dir / summary / probe/examples 路径。

## Scope
- `scripts/run_wave_b_macos_gate.sh`
- `tests/scripts/test_wave_b_macos_gate_dryrun_observability_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 macOS gate dry-run metadata 缺口
- [x] 新增 focused shell contract
- [x] 最小补齐结构化 dry-run 输出
- [x] 跑 focused + 既有 passthrough 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n scripts/run_wave_b_macos_gate.sh tests/scripts/test_wave_b_macos_gate_dryrun_observability_contract.sh tests/scripts/test_wave_b_macos_gate_isolation_passthrough_contract.sh tests/scripts/test_wave_b_macos_gate_fpc_host_passthrough_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_macos_gate_dryrun_observability_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_macos_gate_isolation_passthrough_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_macos_gate_fpc_host_passthrough_contract.sh` => PASS

## Result
- macOS gate dry-run 现在会显式输出：
  - `run_id`
  - `output_dir`
  - `summary`
  - `probe_json`
  - `examples_json`
- 这样 platform gate 的 dry-run 也有了稳定的结构化观察面。

## Next Queue
- 继续扫其它 Wave B producer/consumer 的 dry-run metadata 缺口。
- 或在 repo-noise 更低后回到 backend context/default-validation 架构复审。
