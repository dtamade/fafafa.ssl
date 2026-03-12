# 2026-02-24 Wave B B2 Handoff Bundle 增补 Windows blocker 证据可见性

## Goal

- 在 `scripts/prepare_wave_b_b2_handoff_bundle.sh` 产出的 handoff bundle 中显式展示 `windows_blocker_batch_report`。
- 该行来源于 consistency 报告的 `Artifact Matrix`，用于确保交接包可直接审计 blocker 证据状态。

## Architecture / Scope

- Script:
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - 在 consistency 报告生成后解析 `windows_blocker_batch_report` 行
  - handoff bundle 的 `## Artifacts` 表追加 blocker 证据行（存在/缺失）
- Contract:
  - `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`
  - 覆盖 missing/pass 两种 blocker 证据状态

## Files

- `docs/plans/2026-02-24-wave-b-b2-handoff-bundle-windows-blocker-artifact-visibility.md`
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`
2. GREEN:
   - 修改 `scripts/prepare_wave_b_b2_handoff_bundle.sh`，把 consistency 的 blocker 行写入 handoff bundle artifact 表。
3. Regression:
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`
   - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
   - `bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round8.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round8.md`
   - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`

## Expected Outputs

- handoff bundle 的 `Artifacts` 表可直接看到 `windows_blocker_batch_report` 的 path 与 exists 状态。
- blocker 缺失时显示 `NO`，存在且 run_id 一致时显示 `YES`。
