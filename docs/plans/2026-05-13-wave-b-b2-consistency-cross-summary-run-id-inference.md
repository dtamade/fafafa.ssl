# 2026-05-13 Wave B/B2 Consistency Cross Summary Run ID Inference

## Goal
收口 `scripts/check_wave_b_b2_evidence_consistency.sh` 的 `RUN_ID` 推导时序缺口，避免在调用者只提供现有 `cross summary + closure report` 时，checker 先生成新的时间戳 `run_id`，再去继承 active custom `linux_summary`，最终把同一批证据误判成 `run_id mismatch`。

## Architecture
- truth source:
  - 显式 `--run-id` 仍然最高优先级
  - 否则如果 `cross summary` 已声明 active `linux_summary`，应该先消费这条事实
  - 再从那份 active Linux summary 解析真实 `run_id`
  - 只有上述路径都不可用时，才回退到时间戳
- consistency 入口需要和同族脚本保持一致：
  - active evidence path 先对齐
  - `run_id` 默认值再跟着 active summary 真值走
- 这次只修 `check_wave_b_b2_evidence_consistency.sh`，不改变 `generate/closure/prepare` 的既有合同。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED contract，复现“只传 `cross summary + closure report`，但已有 active custom `linux_summary`”时的假红灯。
2. 仅在 `check_wave_b_b2_evidence_consistency.sh` 内调整 `RUN_ID` 推导时序，让它能先参考 cross-summary-declared active Linux truth。
3. 跑 focused 合同与同面回归，确认不引入 path/required 语义回退。
4. 更新 working memory，给出简短 review 结论并提交。

## Commands
- `bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`
- `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`
- `git diff --check`

## Expected Outputs
- 新合同在修复前失败，并暴露“`RUN_ID` 被错误生成为当前时间戳”的证据。
- 修复后 `check_wave_b_b2_evidence_consistency.sh --strict` 在同批证据上恢复 `CONSISTENT`。
- 既有 active-path / required-semantics / inactive-probe 合同继续通过。
