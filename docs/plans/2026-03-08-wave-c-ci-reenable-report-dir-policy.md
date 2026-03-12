# 2026-03-08 Wave C CI Re-enable Report Dir Policy

## Goal
把 Wave C CI re-enable / submission-pack 链路的默认输出从 `test-reports/` 收口到 `tmp/`，并修复前一波 local-guard 已迁移后留下的默认输入/输出路径漂移。

## Background
上一波已经把 local-guard 主链（B123/B124/B125/B126/B129/B132/B138/B139/B140/B142/B144）默认输出迁移到 `tmp/wave_c_local_guard_reports`。但下游 CI re-enable 链还在默认读写 `test-reports/`，导致：

- B137 仍从 `test-reports/` 读 B129/B132，默认输入与当前真相不一致
- B143 仍从 `test-reports/` 读 B142 JSON
- B146 同时错读 B138/B142/B144 的旧路径
- B147/B148/B149 默认输出继续扩散到 `test-reports/`

这会让“新默认输出策略”只完成一半：上游写到 `tmp/`，下游却还默认从 `test-reports/` 读，形成链路断裂与 repo noise 回流。

## Scope
- 脚本：
  - `scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh`
  - `scripts/check_wave_c_local_guard_alert_thresholds.sh`
  - `scripts/prepare_wave_c_ci_reenable_submission_pack.sh`
  - `scripts/check_wave_c_ci_reenable_submission_pack.sh`
  - `scripts/generate_wave_c_ci_reenable_approval_brief.sh`
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- 合同：
  - 新增 repo-hygiene 默认路径合同
  - 新增 runtime 默认输出合同
  - 修正依赖旧默认路径的 B149 现有合同
- 文档与 working memory：
  - `docs/plans/2026-03-current-summary.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Architecture
- local-guard 共享目录：`tmp/wave_c_local_guard_reports`
- ci-reenable 共享目录：`tmp/wave_c_ci_reenable_reports`
- 环境变量：`FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR`、`FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR`
- 原则：
  - 同一链路内部默认读写共享稳定目录，而不是 `test-reports/`
  - 保持 `--output` / `--input` 显式参数兼容
  - 对 B149 这类多子输出编排脚本，内部子报告与日志也统一落到共享目录

## RED -> GREEN Plan
1. 新增 `tests/scripts/test_repo_hygiene_wave_c_ci_reenable_tmp_defaults_contract.sh`
2. 新增 `tests/scripts/test_wave_c_ci_reenable_default_reports_runtime_contract.sh`
3. 运行新合同，确认 RED
4. 修复 B137/B143/B146/B147/B148/B149 默认路径
5. 修正受影响的 B149 合同默认路径断言
6. 运行 focused contracts + repo-hygiene batch，确认 GREEN
7. 更新月度汇总与 working memory

## Planned Commands
1. `bash tests/scripts/test_repo_hygiene_wave_c_ci_reenable_tmp_defaults_contract.sh`
2. `bash tests/scripts/test_wave_c_ci_reenable_default_reports_runtime_contract.sh`
3. `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b148_alert_state_summary.sh`
4. `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_audit_alert_headline.sh`
5. `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b149_audit_alert_note_summary.sh`
6. `bash tests/scripts/test_wave_c_ci_reenable_submission_bundle_b147_b148_b149_audit_note_consistency.sh`
7. `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
8. `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
9. `bash scripts/summarize_git_status_noise_draft.sh --output tmp/git_status_noise_summary_current.md`
10. `git diff --check -- <touched files>`

## Expected Outputs
- CI re-enable 默认链路不再向 `test-reports/` 写新报告
- B137/B143/B146/B147/B148/B149 默认输入输出与 local-guard 当前真相一致
- B149 现有合同继续通过，但读取新默认路径
- repo-hygiene batch 纳入新合同
