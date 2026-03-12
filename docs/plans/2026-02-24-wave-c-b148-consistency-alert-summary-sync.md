# 2026-02-24 Wave C B148 同步 Consistency Alert Summary

## Goal

- 在 `scripts/generate_wave_c_ci_reenable_approval_brief.sh`（B148）中同步一致性告警摘要，提升外发审批摘要的审计可读性。
- 告警摘要对齐当前口径：`check_state` + token fail 计数 + `alert_state`。

## Architecture / Scope

- Script:
  - `scripts/generate_wave_c_ci_reenable_approval_brief.sh`
  - 新增参数 `--check FILE`（B147 报告）
  - 新增 `## Consistency Alert Summary` 与 `## Token Failures`
- Orchestrator:
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
  - B148 调用链透传 `--check ${b147_report}`
- Contract:
  - `tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
  - 覆盖 WARN 与 CLEAR 两种场景

## Files

- `scripts/generate_wave_c_ci_reenable_approval_brief.sh`
- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)

1. RED:
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
2. GREEN:
   - 实现 B148 consistency alert summary 并透传 B147 check。
3. Regression:
   - `bash tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_local_guard_batch_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_docs_governance_integration.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_skip_local_guard_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_platform_option.sh`
   - `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_only_skip_semantics.sh`
   - `bash -n scripts/generate_wave_c_ci_reenable_approval_brief.sh scripts/run_wave_c_ci_reenable_submission_bundle.sh tests/scripts/test_generate_wave_c_ci_reenable_approval_brief_consistency_alert_summary.sh`
4. Docs governance strict:
   - `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_20260224_round14.md`
   - `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_all_20260224_round14.md`
5. 回写 `task_plan.md` / `findings.md` / `progress.md`。

## Expected Outputs

- B148 报告包含 consistency 告警摘要，能直接看到 check_state 与 token fail 信息。
- B149 既有链路合同无回退；docs strict round14 继续零噪声零重复。
