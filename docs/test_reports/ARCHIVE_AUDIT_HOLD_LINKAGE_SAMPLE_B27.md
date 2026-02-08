# Archive Audit & Hold Expiry Linkage（Draft）

## 1) Metadata

| field | value |
|------|-------|
| linkage_id | b27_sample_20260207_0612 |
| generated_at | 2026-02-07 05:28:21 +0800 |
| sampling_record | docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_SAMPLE_B23.md |
| hold_review_record | docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md |
| operator | codex |

## 2) Linkage Summary

| metric | value |
|--------|-------|
| sampled_runs_total | 1 |
| sampled_runs_with_hold_review | 0 |
| sampled_runs_missing_hold_review | 0 |
| sampled_runs_risk | 0 |
| status | pass |

## 3) Sampled Run Linkage Rows

| run_id | sample_hold | review_status | expires_on | days_left | owner | reason | action |
|--------|-------------|---------------|------------|-----------|-------|--------|--------|
| b11_smoke_20260207_0420 | no | n/a | n/a | n/a | unknown | not-linked | n/a |

## 4) Risk Checklist

- [ ] `overdue` 条目已升级处理。
- [ ] `missing-expiry/invalid-expiry` 条目已补齐日期。
- [ ] `not-found` 且 `sample_hold=yes` 条目已复核元数据同步。

## 5) Attachments

- <archive_audit_sampling_record_path>
- <hold_expiry_review_report_path>
- <followup_ticket_or_log_path>
