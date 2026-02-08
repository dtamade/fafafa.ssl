# Hold Expiry Review Reminder（Draft）

## 1) Metadata

| field | value |
|------|-------|
| generated_at | 2026-02-07 05:04:21 +0800 |
| artifact_root | /tmp/tmp.9zptMDGj6J |
| today | 2026-02-07 |
| lookahead_days | 7 |

## 2) Summary

| metric | value |
|--------|-------|
| total_holds | 3 |
| overdue | 1 |
| due_soon | 1 |
| missing_expiry | 1 |
| invalid_expiry | 0 |

## 3) Hold Review Rows

| run_id | expires_on | days_left | status | owner | reason | meta_path |
|--------|------------|-----------|--------|-------|--------|-----------|
| run_due | 2026-02-10 | 3 | due-soon | qa | release verification | /tmp/tmp.9zptMDGj6J/run_due/.hold.meta |
| run_missing | n/a | n/a | missing-expiry | audit | audit waiting | /tmp/tmp.9zptMDGj6J/run_missing/.hold.meta |
| run_overdue | 2026-02-05 | -2 | overdue | secops | incident investigation | /tmp/tmp.9zptMDGj6J/run_overdue/.hold.meta |

## 4) Next Actions

- 对 `overdue` 条目执行复核或续期。
- 对 `missing-expiry` 与 `invalid-expiry` 条目补齐规范日期。
- 将复核结果回写到对应 `.hold.meta` 与审计记录。
