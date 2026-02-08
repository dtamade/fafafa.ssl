# Archive Audit Approval Chain SLA Breach Alert Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| alert_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| approval_chain_report | `<path>` |
| anomaly_response_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| approval_status | `<pass/warn/fail/unknown>` |
| rejected_stages | `<n>` |
| pending_review_stages | `<n>` |
| chain_release_decision | `<decision>` |
| anomaly_response_status | `<pass/warn/fail/unknown>` |
| anomalies_total | `<n>` |
| critical_high_open | `<n>` |
| queue_items | `<n>` |

## 3) SLA Alert Summary

| metric | value |
|--------|-------|
| total_alert_items | `<n>` |
| critical_alert_items | `<n>` |
| high_alert_items | `<n>` |
| medium_alert_items | `<n>` |
| owner_hotspots | `<n>` |
| sla_breach_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Alert Rows

| alert_id | source | owner | target_sla | target_minutes | observed | alert_level | escalation_action |
|----------|--------|-------|------------|----------------|----------|-------------|-------------------|
| `<ALERT-001>` | `<source>` | `<owner>` | `<sla>` | `<n>` | `<observed>` | `<ok/watch/breach-risk-medium/breach-risk-high>` | `<action>` |

## 5) Owner Hotspots

| owner | critical_open | high_open | medium_open | queue_total | recommended_window |
|-------|---------------|-----------|-------------|-------------|--------------------|
| `<owner>` | `<n>` | `<n>` | `<n>` | `<n>` | `<window>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
