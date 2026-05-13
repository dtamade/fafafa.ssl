# 2026-05-14 Wave B/B2 Handoff Bundle Windows Artifact List

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 的 handoff bundle artifact 清单缺口，避免它在 Windows summary 已激活 companion runtime logs 语义时，bundle 本身却仍漏列 `winssl_quick_smoke` / `winssl_runtime_suite`。

## Architecture
- `prepare` 现在已经会在 Windows summary active/explicit 时推导 companion log 路径并传给 consistency
- handoff bundle 作为交接索引，也必须列出同一组 artifact，不能只列 `windows_summary`
- artifact list 应与 `WINDOWS_EVIDENCE_ARGS` 对齐：
  - Windows summary active/explicit时，列出 quick/runtime rows
  - 缺失时显示 `NO`
  - 存在时显示 `YES`

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 扩 focused RED contracts，证明 handoff bundle artifact list 仍漏列 Windows companion runtime artifacts。
2. 仅在 `prepare_wave_b_b2_handoff_bundle.sh` 内把 Windows companion artifacts 加入 bundle 清单。
3. 跑 focused 合同、显式缺失 passthrough、Windows companion、consistency 邻近回归。
4. 更新 working memory，review 后提交。

## Commands
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
- `git diff --check`

## Expected Outputs
- handoff bundle 在 Windows summary active/explicit 场景下会列出 quick/runtime artifacts。
- 现有 explicit-missing evidence batch 也会在 bundle 索引里显示 companion logs 的缺失状态。
- consistency/prepare/generate 已有合同继续通过。
