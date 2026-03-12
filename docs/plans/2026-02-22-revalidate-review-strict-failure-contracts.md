# 2026-02-22 Revalidate/Review Strict Failure Contracts

## Goal
- 收口 `revalidate/review` 两个 draft 脚本在 `--strict` 下的失败语义，确保与 usage 文案一致且可回归验证。

## Architecture / Scope
- 仅修改严格模式判定逻辑，不改报告结构与主流程。
- 使用合同测试锁定边界：
  - `revalidate`: `revalidation_status != pass` 时严格失败。
  - `review`: 检测到漂移（`drift_percent >= threshold`）时严格失败。

## Files
- `scripts/revalidate_closure_gate_after_autofix_draft.sh`
- `scripts/review_closure_gate_weekly_trend_drift_draft.sh`
- `tests/scripts/test_revalidate_closure_gate_after_autofix_strict_contract.sh`
- `tests/scripts/test_review_closure_gate_weekly_trend_drift_strict_contract.sh`

## Step-by-step Commands
1. RED: 先跑新增严格失败测试，确认当前实现未满足契约。
   - `bash tests/scripts/test_revalidate_closure_gate_after_autofix_strict_contract.sh`
   - `bash tests/scripts/test_review_closure_gate_weekly_trend_drift_strict_contract.sh`
   - 预期：均失败。
2. GREEN: 最小修复 strict 判定。
   - `scripts/revalidate_closure_gate_after_autofix_draft.sh`: strict 下 `status != pass` 退出非 0。
   - `scripts/review_closure_gate_weekly_trend_drift_draft.sh`: strict 下 `drift_percent >= threshold` 退出非 0。
3. Regression: 回归新增测试与相关批量测试。
   - `bash tests/scripts/test_revalidate_closure_gate_after_autofix_strict_contract.sh`
   - `bash tests/scripts/test_review_closure_gate_weekly_trend_drift_strict_contract.sh`
   - `bash tests/scripts/test_archive_phase4_remaining_strict_contracts_batch.sh`
   - `bash -n scripts/revalidate_closure_gate_after_autofix_draft.sh`
   - `bash -n scripts/review_closure_gate_weekly_trend_drift_draft.sh`
   - `bash -n tests/scripts/test_revalidate_closure_gate_after_autofix_strict_contract.sh`
   - `bash -n tests/scripts/test_review_closure_gate_weekly_trend_drift_strict_contract.sh`
   - 预期：全部通过。

## Expected Outputs
- 新增严格边界合同测试可稳定复现 RED，并在修复后 GREEN。
- 既有批量 strict 合同无回归。
