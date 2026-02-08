# Archive Audit Dashboard Threshold Policy Report Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| policy_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| dashboard_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Threshold Configuration

| threshold | value |
|-----------|-------|
| due_soon_warn_threshold | `<n>` |
| blocking_high_threshold | `<n>` |
| checklist_warn_threshold | `<n>` |

## 3) Metric Evaluation

| metric | value | threshold_rule | severity | owner | sla | action |
|--------|-------|----------------|----------|-------|-----|--------|
| `<metric_name>` | `<value>` | `<rule>` | `<low/medium/high/critical>` | `<owner>` | `<sla>` | `<action>` |

## 4) Escalation Summary

| metric | value |
|--------|-------|
| escalation_level | `<low/medium/high/critical>` |
| decision_status | `<pass/warn/fail>` |
| release_policy | `<proceed/proceed-with-mitigation/hold/block-release>` |
| critical_signals | `<n>` |
| high_signals | `<n>` |
| medium_signals | `<n>` |

## 5) Suggested Escalation Runbook

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
