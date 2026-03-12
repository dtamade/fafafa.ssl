# 2026-02-24 Wave B B2 Evidence Consistency 兼容 Windows Blocker 证据联动

## Goal

- 在 `scripts/check_wave_b_b2_evidence_consistency.sh` 中补齐 Windows blocker 证据联动检查：
  - 当 `windows_summary` 包含 `winssl_blocker_batch` 且 evidence 非 `<none>` 时，校验 blocker 报告存在且 run_id 一致。
  - 当 `winssl_blocker_batch` 为 `SKIPPED` 且 evidence 为 `<none>` 时，不强制 blocker 报告。
- 保持历史/旧布局摘要兼容（缺少该行时不误判必需）。

## Architecture / Scope

- Script:
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
  - 新增 Windows summary 解析：
    - `winssl_blocker_batch` 的 status/evidence
  - 新增 Artifact Matrix 行：
    - `windows_blocker_batch_report`（按摘要引用路径检查）
- Contract:
  - `tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
  - 覆盖 PASS 需证据、SKIPPED 不需证据两种语义。

## Files

- `docs/plans/2026-02-24-wave-b-b2-evidence-consistency-windows-blocker-linkage.md`
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
2. GREEN:
   - 修改 `scripts/check_wave_b_b2_evidence_consistency.sh`，增加 blocker 证据联动判定与 Artifact Matrix 行。
3. Regression:
   - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round7.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round7.md`
   - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`

## Expected Outputs

- `check_wave_b_b2_evidence_consistency.sh` 在 Windows summary 引用 blocker 报告时可正确校验证据存在与 run_id 一致性。
- `winssl_blocker_batch` 为 `SKIPPED` 时不误判 blocker 报告缺失。
