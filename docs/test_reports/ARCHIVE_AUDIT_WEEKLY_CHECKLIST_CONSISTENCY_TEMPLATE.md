# Archive Audit Weekly vs Checklist Consistency Report Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| consistency_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| weekly_report | `<path>` |
| checklist_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| checklist_readiness | `<pass/warn/fail/unknown>` |
| checklist_blocking_reasons | `<none_or_reason_list>` |
| checklist_hold_overdue | `<n>` |
| weekly_status | `<pass/warn/fail/unknown>` |
| weekly_checklist_fail | `<n>` |
| weekly_checklist_warn | `<n>` |
| weekly_hold_overdue_total | `<n>` |
| weekly_linkage_risk_total | `<n>` |
| weekly_checklist_inputs | `<n>` |

## 3) Consistency Summary

| metric | value |
|--------|-------|
| total_checks | `<n>` |
| passed_checks | `<n>` |
| critical_fail_count | `<n>` |
| warning_count | `<n>` |
| consistency_status | `<pass/warn/fail>` |
| release_recommendation | `<recommendation>` |

## 4) Consistency Checks

| check_id | level | result | expected | actual | note |
|----------|-------|--------|----------|--------|------|
| `<check_id>` | `<critical/warning>` | `<pass/fail>` | `<expected>` | `<actual>` | `<note>` |

## 5) Source Row Match

| item | value |
|------|-------|
| weekly_row_found | `<true/false>` |
| weekly_row_source | `<source_path_or_na>` |
| weekly_row_readiness | `<value>` |
| weekly_row_blocking_reasons | `<value>` |

## 6) Suggested Actions

- blocking:
  - `<blocking_action_1>`
- followup:
  - `<followup_action_1>`
