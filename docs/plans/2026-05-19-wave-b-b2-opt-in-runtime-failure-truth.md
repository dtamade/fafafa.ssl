# Wave B/B2 Opt-In Runtime Failure Truth

## Goal

收口 `Wave B/B2` 报告链对 WinSSL opt-in runtime failure 的真相传播，避免 GitHub workflow 已经 `windows-gate` 失败、`winssl_runtime_suite_<run_id>.log` 已明确 `suite_end_status=FAIL` 时：

- `wave_b_cross_platform_summary_<run_id>.md` 仍继续写 `windows | PASS`
- `wave_b_b2_handoff_bundle_<run_id>.md` 仍继续写 `handoff_state: CLOSED`

## Scope

- `scripts/generate_wave_b_cross_platform_summary.sh`
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `src/fafafa.ssl.winssl.connection.pas`
- 不重开 native probe 安全实现
- 不把 `check_wave_b_b2_evidence_consistency.sh` 的 `CONSISTENT` 语义整体改成“所有 runtime 都必须 PASS”

## Why This Batch

当前 live run `26068984446` 已经证明一条具体流程 bug：

- GitHub run 结论：`FAILURE`
- Windows runtime transcript：
  - `WinSSL Session Resumption Truth` 退出码 `-1073741819`
  - `suite_end_status=FAIL`
- 但同一批生成的：
  - `wave_b_cross_platform_summary_*.md`
  - `wave_b_b2_handoff_bundle_*.md`
  仍继续给出普通 `windows PASS` / `CLOSED`

这会直接误导后续验证与路线判断，因此应先修顶层报告真相，再继续 native probe 实现调查。

## Planned Changes

1. 新增 cross summary focused contract：
   - 当显式提供 Windows runtime transcript 且其中 `suite_end_status=FAIL` 时，Windows platform state 必须提升成 `FAIL`
2. 新增 handoff bundle focused contract：
   - 当 sibling Windows runtime transcript 已明确 `suite_end_status=FAIL` 时，handoff state 不能继续 `CLOSED`
3. 最小修改 `generate_wave_b_cross_platform_summary.sh`：
   - 支持可选 `--windows-runtime-transcript`
   - 仅在 transcript 明确 `suite_end_status=FAIL` 时，把 Windows state 提升为 `FAIL`
4. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`：
   - 把 sibling Windows runtime transcript 传给 cross summary
   - 若 transcript 明确 `suite_end_status=FAIL`，则顶层 handoff state 至少落到 `NEEDS_GATE_REPAIR`

## Verification

```bash
bash -n tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_windows_runtime_fail_contract.sh
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh
git diff --check
```

## Expected Outcome

- opt-in Windows runtime failure 不再被顶层 summary / handoff bundle 伪装成普通 green closure
- 后续做 native probe / risky runtime 调查时，报告链能更快反映真实失败边界
