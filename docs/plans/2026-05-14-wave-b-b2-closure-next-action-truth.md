# Wave B/B2 Closure Next Action Truth

## Goal
让 closure readiness 报告的 `Next Actions` 对齐当前的 `prepare_wave_b_b2_handoff_bundle.sh` 单一交接入口，避免继续提示一个已经不再负责完整收口的旧脚本。

## Why This Batch
- closure report 仍提示复跑 `generate_wave_b_cross_platform_summary.sh`
- 但当前完整的收口链已经是 `prepare -> cross/closure/consistency/handoff bundle`
- 旧提示会让调用者只刷新 cross summary，而漏掉 consistency 与 handoff bundle

## Files
- `scripts/check_wave_b_b2_closure_readiness.sh`
- `tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused closure contract，要求 closure report 不再提示旧的 `generate` 重跑入口。
2. 让合同先在当前脚本上 RED。
3. 最小同步 closure report 的最后一步文案到 `prepare` handoff 入口。
4. 跑 focused closure/handoff 回归与 `git diff --check`。
5. 更新 working-memory，给出简短 review 结论并提交。

## Expected Verification
- `bash -n tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`
- `bash tests/scripts/test_wave_b_b2_closure_next_actions_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`
- `git diff --check`
