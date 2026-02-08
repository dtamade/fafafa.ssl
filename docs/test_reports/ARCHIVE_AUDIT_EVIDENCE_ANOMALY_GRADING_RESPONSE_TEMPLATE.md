# Archive Audit Evidence Anomaly Grading & Response Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| response_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| audit_report | `<path>` |
| adaptive_policy_report | `<path>` |
| versioning_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| audit_status | `<pass/warn/fail/unknown>` |
| audit_checks_fail | `<n>` |
| audit_release_advice | `<advice>` |
| adaptive_status | `<pass/warn/fail/unknown>` |
| adaptation_mode | `<tighten/reinforce/hold/relax>` |
| pressure_score | `<n>` |
| adaptive_release_guidance | `<guidance>` |
| versioning_status | `<pass/warn/fail/unknown>` |
| rollback_candidates | `<n>` |
| versioning_release_advice | `<advice>` |

## 3) Grading Summary

| metric | value |
|--------|-------|
| anomalies_total | `<n>` |
| critical_count | `<n>` |
| high_count | `<n>` |
| medium_count | `<n>` |
| low_count | `<n>` |
| critical_high_open | `<n>` |
| response_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Anomaly Rows

| anomaly_id | source | severity | owner | sla | anomaly_key | observed | disposition | response_action | evidence |
|------------|--------|----------|-------|-----|-------------|----------|-------------|-----------------|----------|
| `<A-001>` | `<source>` | `<severity>` | `<owner>` | `<sla>` | `<key>` | `<observed>` | `<open/queued/closed>` | `<action>` | `<evidence>` |

## 5) Response Queue

| anomaly_id | severity | owner | sla | immediate_action | status |
|------------|----------|-------|-----|------------------|--------|
| `<A-001>` | `<severity>` | `<owner>` | `<sla>` | `<action>` | `<open/queued/closed>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
