# 2026-05-19 Wave B/B2 Closure Windows Runtime Truth

## Goal
收口 `check_wave_b_b2_closure_readiness.sh` 仍只信 Windows summary 的漂移，让它在 `winssl_runtime_suite_<run_id>.log` 明确 `suite_end_status=FAIL` 时，不再把 Windows closure state 误报成 `PASS`。

## Scope
- `scripts/check_wave_b_b2_closure_readiness.sh`
- `tests/scripts/test_wave_b_b2_closure_windows_runtime_fail_contract.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：
- 不修改 `src/fafafa.ssl.winssl.connection.pas`
- 不继续扩 WinSSL native probe 实现调查
- 不重写 `consistency` / `handoff` 语义

## Why This Batch
当前 live run `26070488337` 已经证明：

- `cross summary` 会把 Windows 从 `summary overall=PASS` 提升成 `FAIL`
- `handoff bundle` 也会落到 `NEEDS_GATE_REPAIR`
- 但同一批 `closure_readiness` 仍写成 `windows | PASS`

这会导致同一条 report chain 内部互相打架，后续阅读 artifact 时仍可能误以为 closure 层已经闭环。

## Planned Changes
1. 先写 focused RED 合同：
   - `check_wave_b_b2_closure_readiness.sh` 在 sibling runtime transcript 为 `FAIL` 时，必须把 Windows state 降成 `FAIL`
   - `prepare_wave_b_b2_handoff_bundle.sh` 生成的 closure report 也必须继承这条 truth
2. 最小修改 `check_wave_b_b2_closure_readiness.sh`：
   - 新增可选 `--windows-runtime-transcript`
   - 若未显式传入且存在 `--windows-summary`，默认查找 sibling `winssl_runtime_suite_<run_id>.log`
   - transcript 只负责把 Windows state 从 `PASS/READY/PENDING` 降级为 `FAIL`，不负责把缺 summary 的情况提升成 `PASS`
3. 复跑 closure/handoff 邻近 shell 合同。
4. 更新 working memory，记录这次新的流程裂缝与修复边界。

## Verification
```bash
bash -n scripts/check_wave_b_b2_closure_readiness.sh
bash -n tests/scripts/test_wave_b_b2_closure_windows_runtime_fail_contract.sh
bash tests/scripts/test_wave_b_b2_closure_windows_runtime_fail_contract.sh
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_contract.sh
bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh
git diff --check
```

## Expected Outcome
- closure report 不再把 `runtime transcript FAIL` 的 Windows 批次误写成 `PASS`
- handoff bundle 生成出来的四层报告：
  - cross summary
  - closure readiness
  - evidence consistency
  - handoff bundle
  对这条 Windows truth 的结论重新对齐
