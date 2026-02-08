# Archive Audit & Hold Expiry Linkage Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| linkage_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| sampling_record | `<path>` |
| hold_review_record | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Linkage Summary

| metric | value |
|--------|-------|
| sampled_runs_total | `<n>` |
| sampled_runs_with_hold_review | `<n>` |
| sampled_runs_missing_hold_review | `<n>` |
| sampled_runs_risk | `<n>` |
| status | `<pass/warn/fail>` |

## 3) Sampled Run Linkage Rows

| run_id | sample_hold | review_status | expires_on | days_left | owner | reason | action |
|--------|-------------|---------------|------------|-----------|-------|--------|--------|
| `<run_id>` | `<yes/no/unknown>` | `<ok/due-soon/overdue/missing-expiry/invalid-expiry/not-found/n/a>` | `<date_or_n/a>` | `<n_or_n/a>` | `<owner>` | `<reason>` | `<action>` |

## 4) Risk Checklist

- [ ] `overdue` 条目已升级处理。
- [ ] `missing-expiry/invalid-expiry` 条目已补齐日期。
- [ ] `not-found` 且 `sample_hold=yes` 条目已复核元数据同步。

## 5) Attachments

- `<archive_audit_sampling_record_path>`
- `<hold_expiry_review_report_path>`
- `<followup_ticket_or_log_path>`
