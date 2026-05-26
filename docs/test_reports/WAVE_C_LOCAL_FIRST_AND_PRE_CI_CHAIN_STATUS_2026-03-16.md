# Wave C Local-first and Pre-CI Chain Status（2026-03-16）

## Goal

给当前 Wave C 的 local-first / pre-CI / submission 链路提供一个统一入口，避免继续把 2026-02-08/2026-02-09 的历史结果页当成默认导航。

## Current Default Entry

- closeout status：`docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`
- baseline/readiness manifest：`docs/test_reports/WAVE_C_BASELINE_READINESS_MANIFEST_2026-03-19.md`
- latest live evidence manifest：`docs/test_reports/WAVE_C_LIVE_EVIDENCE_MANIFEST_2026-03-19.md`
- approval/submission manifest：`docs/test_reports/WAVE_C_APPROVAL_SUBMISSION_MANIFEST_2026-03-19.md`
- 基线与执行入口：`docs/test_reports/WAVE_C_UNIFIED_BASELINE_STATUS_2026-03-15.md`
- readiness 与阈值刷新：`docs/test_reports/WAVE_C_READINESS_REFRESH_2026-03-15.md`、`docs/test_reports/WAVE_C_UNIFIED_THRESHOLD_REFRESH_2026-03-15.md`
- approval / enablement：`docs/test_reports/WAVE_C_B113_SIGNOFF_PACK_RESULT_2026-03-15.md`、`docs/test_reports/WAVE_C_B115_WORKFLOW_ENABLE_RESULT_2026-03-15.md`、`docs/test_reports/WAVE_C_B116_ENABLEMENT_PACKET_RESULT_2026-03-15.md`
- submission / approval brief：`docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-03-16.md`、`docs/test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_RESULT_2026-03-16.md`、`docs/test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-03-15.md`、`docs/test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-03-16.md`

## Latest tmp/test-reports Evidence Batch

- 当前最新的同批次 local-first / pre-CI / ops-pack 实链证据来自 `run_id = 20260319_consistent_b144`。
- 这批证据覆盖 `B123 -> B144`，比此前页面里保留的 `20260316_live` / `20260316_guard2` 样例更适合作为当前默认巡检索引。
- 如果你要按模块查看当前证据，直接跳到 `docs/test_reports/WAVE_C_LIVE_EVIDENCE_MANIFEST_2026-03-19.md`。

| step                  | status               | current evidence                                                                       |
| --------------------- | -------------------- | -------------------------------------------------------------------------------------- |
| B123 local continuity | `LOCAL_READY`        | `tmp/test-reports/wave_c_b123_local_first_continuity_20260319_consistent_b144.md`      |
| B124 drift watch      | `LOCAL_STABLE`       | `tmp/test-reports/wave_c_b124_local_drift_watch_20260319_consistent_b144.md`           |
| B125 guard bundle     | `PASS`               | `tmp/test-reports/wave_c_b125_local_guard_bundle_20260319_consistent_b144.md`          |
| B126 history trend    | `STABLE`             | `tmp/test-reports/wave_c_b126_local_guard_history_20260319_consistent_b144.md`         |
| B129 oncall check     | `PASS`               | `tmp/test-reports/wave_c_b129_oncall_check_20260319_consistent_b144.md`                |
| B132 status snapshot  | `GREEN`              | `tmp/test-reports/wave_c_b132_local_first_status_snapshot_20260319_consistent_b144.md` |
| B137 pre-CI packet    | `READY_FOR_APPROVAL` | `tmp/test-reports/wave_c_b137_pre_ci_reenable_packet_20260319_consistent_b144.md`      |
| B138 pre-CI full gate | `PASS`               | `tmp/test-reports/wave_c_b138_pre_ci_reenable_full_gate_20260319_consistent_b144.md`   |
| B139 cleanup plan     | `DRY_RUN, 0 deleted` | `tmp/test-reports/wave_c_b139_local_guard_cleanup_plan_20260319_consistent_b144.md`    |
| B140 consistency      | `CONSISTENT`         | `tmp/test-reports/wave_c_b140_local_guard_consistency_20260319_consistent_b144.md`     |
| B142 status export    | `HEALTHY`            | `tmp/test-reports/wave_c_b142_local_guard_status_20260319_consistent_b144.json`        |
| B143 alert thresholds | `NONE`               | `tmp/test-reports/wave_c_b143_alert_thresholds_20260319_consistent_b144.md`            |
| B144 ops pack         | `PASS`               | `tmp/test-reports/wave_c_b144_local_guard_ops_pack_20260319_consistent_b144.md`        |

## Current Approval and Submission Docs

- 如果你要按审批模块查看当前材料，直接跳到 `docs/test_reports/WAVE_C_APPROVAL_SUBMISSION_MANIFEST_2026-03-19.md`。

| step                   | status               | current document                                                                   |
| ---------------------- | -------------------- | ---------------------------------------------------------------------------------- |
| B146 submission pack   | `READY_TO_SUBMIT`    | `docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_RESULT_2026-03-16.md`   |
| B147 pack check        | `PASS`               | `docs/test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_RESULT_2026-03-16.md`         |
| B148 approval brief    | `READY_FOR_APPROVAL` | `docs/test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_RESULT_2026-03-15.md`    |
| B149 submission bundle | `PASS`               | `docs/test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_RESULT_2026-03-16.md` |

## How To Use Historical Pages

- `docs/test_reports/WAVE_C_B123_*` 到 `docs/test_reports/WAVE_C_B145_*` 继续保留 2026-02-09 的历史样例，主要用于对照迁移前后的命令、输出格式和阶段结论。
- `docs/test_reports/WAVE_C_B146_*` 到 `docs/test_reports/WAVE_C_B149_*` 同时保留历史版与新版；当前默认应优先看 2026-03-15/2026-03-16 版本。
- 如果只想找到当前有效入口，直接从本页跳到对应的 2026-03-15/2026-03-16 文档，不再从旧页反推。

## Historical Guidance Pages Still Useful

- `docs/test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md`：保留最早的一页式操作顺序，适合回看历史 trigger 语义。
- `docs/test_reports/WAVE_C_B122_CI_DEFERRED_LOCAL_MODE_2026-02-08.md`：保留“先 disable、后 local-first”这一治理决策的原始记录。
- `docs/test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md`：保留 local-first 故障定位顺序，但阅读时应结合当前 `tmp/test-reports` 口径。
- `docs/test_reports/WAVE_C_B130_ONCALL_RHYTHM_TEMPLATE_2026-02-09.md`：保留 oncall 节奏模板，适合继续复用到当前守护链。
- `docs/test_reports/WAVE_C_B131_LOCAL_FIRST_HANDOFF_CHECKLIST_2026-02-09.md`：保留最小交接清单，适合值班接手时快速复核。

## Recommendation

- 文档导航默认先看 `docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`，再按 `baseline -> readiness -> signoff -> packet -> submission bundle` 的顺序阅读。
- 只有在需要核对历史样例、迁移差异或旧路径兼容性时，再回看 2026-02-08/2026-02-09 页面。
