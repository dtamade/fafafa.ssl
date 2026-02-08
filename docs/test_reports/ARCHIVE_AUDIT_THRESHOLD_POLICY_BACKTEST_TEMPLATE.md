# Archive Audit Threshold Policy Backtest & Drift Monitor Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| backtest_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| dashboard_glob | `<glob_pattern>` |
| total_runs | `<n>` |
| operator | `<name_or_ci_job>` |

## 2) Threshold Configuration

| threshold | value |
|-----------|-------|
| due_soon_warn_threshold | `<n>` |
| blocking_high_threshold | `<n>` |
| checklist_warn_threshold | `<n>` |
| drift_alert_threshold | `<n>` |

## 3) Backtest Summary

| metric | value |
|--------|-------|
| critical_runs | `<n>` |
| high_runs | `<n>` |
| medium_runs | `<n>` |
| low_runs | `<n>` |
| avg_hold_due_soon_total | `<n>` |
| avg_blocking_reason_total | `<n>` |
| avg_checklist_readiness_fail | `<n>` |
| drift_alerts | `<n>` |
| backtest_status | `<pass/warn/fail>` |
| release_guidance | `<guidance>` |

## 4) Per-Run Evaluation

| source | escalation_level | hold_overdue_total | hold_due_soon_total | hold_missing_or_invalid_expiry_total | checklist_fail | checklist_warn | weekly_fail_count | linkage_risk_total | blocking_reason_total |
|--------|------------------|--------------------|---------------------|--------------------------------------|----------------|----------------|-------------------|--------------------|-----------------------|
| `<dashboard_path>` | `<low/medium/high/critical>` | `<n>` | `<n>` | `<n>` | `<n>` | `<n>` | `<n>` | `<n>` | `<n>` |

## 5) Drift Monitor

| metric | first_run | last_run | absolute_diff | drift_status |
|--------|-----------|----------|---------------|--------------|
| `<metric_name>` | `<n>` | `<n>` | `<n>` | `<ok/alert>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
