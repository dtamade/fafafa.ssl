# Archive Audit Status Dashboard Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| dashboard_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| hold_report_inputs | `<n>` |
| linkage_report_inputs | `<n>` |
| checklist_report_inputs | `<n>` |
| weekly_report_inputs | `<n>` |
| operator | `<name_or_ci_job>` |

## 2) Dashboard Snapshot

| metric | value |
|--------|-------|
| dashboard_status | `<pass/warn/fail>` |
| hold_status | `<pass/warn/fail/n/a>` |
| linkage_status | `<pass/warn/fail/n/a>` |
| checklist_status | `<pass/warn/fail/n/a>` |
| weekly_status | `<pass/warn/fail/n/a>` |
| hold_overdue_total | `<n>` |
| hold_due_soon_total | `<n>` |
| hold_missing_or_invalid_expiry_total | `<n>` |
| linkage_risk_total | `<n>` |
| checklist_readiness_fail | `<n>` |
| checklist_readiness_warn_or_unknown | `<n>` |
| weekly_fail_count | `<n>` |
| weekly_warn_or_unknown_count | `<n>` |
| blocking_reason_total | `<n>` |

## 3) Signal Board

| dimension | status | key_metrics | evidence |
|-----------|--------|-------------|----------|
| hold_expiry | `<pass/warn/fail/n/a>` | `overdue=<n>; due_soon=<n>; missing_or_invalid=<n>` | `<hold_report_path_or_na>` |
| audit_linkage | `<pass/warn/fail/n/a>` | `sampled_runs_risk_total=<n>` | `<linkage_report_path_or_na>` |
| release_checklist | `<pass/warn/fail/n/a>` | `fail=<n>; warn_or_unknown=<n>` | `<checklist_report_path_or_na>` |
| weekly_execution | `<pass/warn/fail/n/a>` | `fail=<n>; warn_or_unknown=<n>` | `<weekly_report_path_or_na>` |
| overall_dashboard | `<pass/warn/fail>` | `inputs=<n>; blocking_reasons=<n>` | `<dashboard_output_path>` |

## 4) Hold Aggregate Detail

| source | overdue | due_soon | missing_expiry | invalid_expiry | row_status |
|--------|---------|----------|----------------|----------------|------------|
| `<hold_report_path>` | `<n>` | `<n>` | `<n>` | `<n>` | `<pass/warn/fail>` |

## 5) Linkage Aggregate Detail

| source | sampled_runs_risk | source_status | row_status |
|--------|-------------------|---------------|------------|
| `<linkage_report_path>` | `<n>` | `<pass/warn/fail/unknown>` | `<pass/warn/fail>` |

## 6) Checklist Aggregate Detail

| source | readiness | blocking_reasons |
|--------|-----------|------------------|
| `<checklist_report_path>` | `<pass/warn/fail>` | `<none_or_reason_list>` |

## 7) Weekly Trend

| source | weekly_status | hold_overdue_total | checklist_readiness_fail |
|--------|---------------|--------------------|--------------------------|
| `<weekly_report_path>` | `<pass/warn/fail>` | `<n>` | `<n>` |

## 8) Blocking Reason Aggregate

| reason | count |
|--------|-------|
| `<blocking_reason_or_none>` | `<n>` |

## 9) Suggested Actions

- blocking:
  - `<blocking_action_1>`
- followup:
  - `<followup_action_1>`
