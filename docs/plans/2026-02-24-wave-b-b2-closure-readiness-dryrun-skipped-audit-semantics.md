# 2026-02-24 Wave B B2 Closure Readiness 统一 DRY_RUN/SKIPPED 审计口径

## Goal

- 让 `scripts/check_wave_b_b2_closure_readiness.sh` 对 `DRY_RUN/SKIPPED` 状态有确定识别与可审计文案。
- 保持 strict 语义不变：仅 `closure_status=CLOSED` 视为通过。

## Architecture / Scope

- Script:
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - 新增状态识别：`SKIP/SKIPPED`
  - 报告新增状态语义说明（`PASS/DRY_RUN/SKIPPED/PENDING/READY`）
  - Next Actions 文案统一覆盖 `DRY_RUN/SKIPPED/PENDING/READY`
- Contract:
  - `tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`
  - 覆盖：
    - mixed 状态（linux=PASS/macOS=DRY_RUN/windows=SKIPPED）→ `IN_PROGRESS`
    - strict 在 mixed 状态下失败
    - all PASS 时 strict 通过且 `CLOSED`

## Files

- `docs/plans/2026-02-24-wave-b-b2-closure-readiness-dryrun-skipped-audit-semantics.md`
- `scripts/check_wave_b_b2_closure_readiness.sh`
- `tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`

## Step-by-step Commands

1. RED:
   - `bash tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`
2. GREEN:
   - 修改 `scripts/check_wave_b_b2_closure_readiness.sh` 的状态识别和报告文案。
3. Regression:
   - `bash tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`
   - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_blocker_artifact_visibility.sh`
   - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_blocker_linkage_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_windows_gate_blocker_layout.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_android_extension.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round9.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round9.md`
   - `bash -n scripts/check_wave_b_b2_closure_readiness.sh tests/scripts/test_wave_b_b2_closure_readiness_dryrun_skipped_semantics_contract.sh`

## Expected Outputs

- closure readiness 报告在 mixed 状态下准确显示 `DRY_RUN/SKIPPED`，并保持 `closure_status=IN_PROGRESS`。
- strict 在 mixed 状态失败、在 all PASS 状态通过。
- 报告包含状态语义说明，便于 handoff 审计一致解读。
