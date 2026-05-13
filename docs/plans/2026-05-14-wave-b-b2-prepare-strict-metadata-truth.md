# Wave B/B2 Prepare Strict Metadata Truth

## Goal
让 `prepare_wave_b_b2_handoff_bundle.sh --strict` 生成出来的 closure/consistency/handoff bundle 报告在 `strict_mode` 元数据上保持一致，同时保留“先生成所有报告、最后再严格失败”的执行顺序。

## Why This Batch
- 直接把 `--strict` 传给 closure/consistency 生成调用，会让脚本在生成完整交接包前提前退出
- 但当前不传 `--strict` 的主流程又会让落盘报告把 `strict_mode` 错写成 `false`
- 这是一个真实的证据自相矛盾，而不是展示细节

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `prepare --strict` 下 closure/consistency 的 `strict_mode` 仍是 `false`。
2. 最小修改 `prepare_wave_b_b2_handoff_bundle.sh`：
   - 保持现有生成顺序
   - 仅同步已生成报告的 `strict_mode` 行
3. 跑 focused prepare contracts 与 handoff 邻近回归。
4. 更新 working-memory，给出简短 review 结论并提交。

## Expected Verification
- `bash -n tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- `git diff --check`
