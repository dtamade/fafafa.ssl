# 2026-03-10 continuous test monitor output governance

## Goal
- 收口 `scripts/continuous_test_monitor.sh` 的活动输出面。
- 冻结 `summary` / `history` / `trend` 文件路径，以及 run-scoped `unit/bin` 目录都落在 monitor reports 根内。

## Architecture
- 这波不改 monitor 的统计口径、循环语义、或告警语义。
- 只把 monitor 输出层拆成两块：
  - `REPORTS_DIR/monitor`
    - `test_history.csv`
    - `monitor_summary.txt`
    - `trend_report.txt`
  - `REPORTS_DIR/runs`
    - `continuous_monitor_units_<run_id>`
    - `continuous_monitor_bin_<run_id>`
- 用脚本复制 + fake runner 的 runtime 合同覆盖真实运行路径，而不是只做静态 grep。

## Files
- `scripts/continuous_test_monitor.sh`
- `tests/scripts/test_continuous_test_monitor_output_governance_contract.sh`
- `tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh`
- `tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Commands
1. `bash tests/scripts/test_continuous_test_monitor_output_governance_contract.sh`
2. `bash tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh`
3. `bash -n scripts/continuous_test_monitor.sh tests/scripts/test_continuous_test_monitor_output_governance_contract.sh tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh tests/scripts/test_continuous_test_monitor_isolation_passthrough_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`
4. `bash tests/scripts/test_continuous_test_monitor_output_governance_contract.sh`
5. `bash tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh`
6. `bash tests/scripts/test_continuous_test_monitor_isolation_passthrough_contract.sh`
7. `bash tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`
8. `git diff --check -- scripts/continuous_test_monitor.sh tests/scripts/test_continuous_test_monitor_output_governance_contract.sh tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh`

## Expected Outputs
- RED：static/runtime 合同先失败，说明 `TREND_FILE` / `RUNS_DIR` 缺失，且 run-scoped unit/bin 仍落在 reports 面之外。
- GREEN：monitor 输出层收口到 `REPORTS_DIR/monitor` + `REPORTS_DIR/runs`。
- Verification：focused static/runtime/isolation/repo-hygiene 合同全绿。

## Verification
- `bash tests/scripts/test_continuous_test_monitor_output_governance_contract.sh` => PASS
- `bash tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh` => PASS
- `bash -n scripts/continuous_test_monitor.sh tests/scripts/test_continuous_test_monitor_output_governance_contract.sh tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh tests/scripts/test_continuous_test_monitor_isolation_passthrough_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh` => PASS
- `bash tests/scripts/test_continuous_test_monitor_isolation_passthrough_contract.sh` => PASS
- `bash tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh` => PASS
- `git diff --check -- scripts/continuous_test_monitor.sh tests/scripts/test_continuous_test_monitor_output_governance_contract.sh tests/scripts/test_continuous_test_monitor_output_governance_runtime_contract.sh tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh` => PASS

## Result
- monitor 的活动输出现在不会再把 run-scoped unit/bin 目录散落到 reports 面之外。
- `summary/history/trend` 与 run artifacts 的层级关系已经固定下来，后续 archive / hygiene / runtime 合同更容易继续收口。
