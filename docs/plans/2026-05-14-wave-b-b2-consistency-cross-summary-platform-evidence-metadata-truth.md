# 2026-05-14 Wave B/B2 Consistency Cross Summary Platform Evidence Metadata Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 `cross_summary` 平台 evidence 行的盲信，避免 macOS probe / Windows summary 被标成 active 但路径 metadata 已丢失时，strict consistency 仍错误产出 `CONSISTENT`。

## Architecture
- `cross_summary` 不只是一个带 `run_id` 的摘要文件，它还承担 active platform evidence truth：
  - macOS: `summary:` 或 `probe:`
  - Windows: `summary:`
- `check_wave_b_b2_evidence_consistency.sh` 之前虽然会继承路径，但只在“路径仍可解析”时生效。
- 一旦 `cross_summary` 的 active evidence 行只剩 state、路径 metadata 丢了，consistency 就会静默退回默认路径并继续给绿灯。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh`
- `tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 active macOS probe metadata lost / active Windows summary metadata lost 两种 `cross_summary` 场景。
2. 证明当前 strict consistency 仍继续返回 `CONSISTENT`。
3. 最小修改 `check_wave_b_b2_evidence_consistency.sh`：
   - 解析 `cross_summary` 平台 state/evidence
   - 校验 active macOS/Windows evidence metadata 是否完整
   - 让显式 `probe: <path> (missing file)` 也被正确继承并要求
4. 复跑 consistency 与 prepare/handoff 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：active platform evidence metadata 已坏，但 consistency 仍给 `CONSISTENT`。
- 修复后：
  - active macOS probe / Windows summary metadata loss 会落到 `INCONSISTENT`
  - `cross_summary` 行显式写出 parse issue
  - 显式缺失的 macOS probe path 也会被 consistency 继承并要求。
