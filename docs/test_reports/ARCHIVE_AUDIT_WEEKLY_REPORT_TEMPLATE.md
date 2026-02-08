# Archive Audit Weekly Report Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| week_id | `<YYYY_wWW_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| hold_report_inputs | `<n>` |
| linkage_report_inputs | `<n>` |
| checklist_report_inputs | `<n>` |
| operator | `<name_or_ci_job>` |

## 2) Weekly Snapshot

| metric | value |
|--------|-------|
| hold_overdue_total | `<n>` |
| hold_due_soon_total | `<n>` |
| hold_missing_or_invalid_expiry_total | `<n>` |
| linkage_risk_total | `<n>` |
| checklist_readiness_fail | `<n>` |
| checklist_readiness_warn | `<n>` |
| weekly_status | `<pass/warn/fail>` |

## 3) Hold Aggregate

| source | overdue | due_soon | missing_expiry | invalid_expiry |
|--------|---------|----------|----------------|----------------|
| `<hold_report_path>` | `<n>` | `<n>` | `<n>` | `<n>` |

## 4) Linkage Aggregate

| source | sampled_runs_risk | status |
|--------|-------------------|--------|
| `<linkage_report_path>` | `<n>` | `<pass/warn/fail>` |

## 5) Checklist Aggregate

| source | readiness | blocking_reasons |
|--------|-----------|------------------|
| `<checklist_report_path>` | `<pass/warn/fail>` | `<none_or_reason_list>` |

## 6) Weekly Actions

- blocking:
  - `<blocking_action_1>`
- followup:
  - `<followup_action_1>`
