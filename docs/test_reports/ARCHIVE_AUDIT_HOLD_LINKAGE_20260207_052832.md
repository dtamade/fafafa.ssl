# Archive Audit & Hold Expiry Linkage（Draft）

## 1) Metadata

| field | value |
|------|-------|
| linkage_id | 20260207_052832 |
| generated_at | 2026-02-07 05:28:32 +0800 |
| sampling_record | /tmp/tmp.Rn33ziRauj |
| hold_review_record | docs/test_reports/HOLD_EXPIRY_REVIEW_2026-02-07.md |
| operator | codex |

## 2) Linkage Summary

| metric | value |
|--------|-------|
| sampled_runs_total | 1 |
| sampled_runs_with_hold_review | 1 |
| sampled_runs_missing_hold_review | 0 |
| sampled_runs_risk | 1 |
| status | warn |

## 3) Sampled Run Linkage Rows

| run_id | sample_hold | review_status | expires_on | days_left | owner | reason | action |
|--------|-------------|---------------|------------|-----------|-------|--------|--------|
| run_overdue | yes | overdue | 2026-02-05 | -2 | secops | incident investigation | immediate-review |

## 4) Risk Checklist

- [ ] `overdue` 条目已升级处理。
- [ ] `missing-expiry/invalid-expiry` 条目已补齐日期。
- [ ] `not-found` 且 `sample_hold=yes` 条目已复核元数据同步。

## 5) Attachments

- <archive_audit_sampling_record_path>
- <hold_expiry_review_report_path>
- <followup_ticket_or_log_path>
