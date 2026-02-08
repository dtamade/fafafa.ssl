# Archive Audit Writeback Change Coverage Remediation Tracker Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| tracker_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| writeback_report | `<path>` |
| linkage_report | `<path>` |
| adaptive_policy_report | `<path>` |
| versioning_report | `<path>` |
| anomaly_response_report | `<path>` |
| sla_alert_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| writeback_status | `<pass/warn/fail/unknown>` |
| writeback_signaled_items | `<n>` |
| writeback_changed_items | `<n>` |
| writeback_pending_items | `<n>` |
| writeback_inprogress_items | `<n>` |
| linkage_status | `<pass/warn/fail/unknown>` |
| mismatch_rows | `<n>` |
| missing_payload_rows | `<n>` |
| adaptive_status | `<pass/warn/fail/unknown>` |
| pressure_score | `<n>` |
| policy_writeback_change_result | `<pass/review/fail/unknown>` |
| versioning_status | `<pass/warn/fail/unknown>` |
| rollback_candidates | `<n>` |
| anomaly_response_status | `<pass/warn/fail/unknown>` |
| critical_high_open | `<n>` |
| sla_breach_status | `<pass/warn/fail/unknown>` |
| critical_alert_items | `<n>` |
| high_alert_items | `<n>` |

## 3) Coverage Tracking Summary

| metric | value |
|--------|-------|
| writeback_change_coverage_percent | `<n>%` |
| unresolved_payload_items | `<n>` |
| total_gap_items | `<n>` |
| remediation_queue_items | `<n>` |
| critical_gap_items | `<n>` |
| high_gap_items | `<n>` |
| medium_gap_items | `<n>` |
| owner_hotspots | `<n>` |
| tracker_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Coverage Gap Rows

| item_id | source | priority | owner | sla | current_status | target_status | remediation_action | evidence |
|---------|--------|----------|-------|-----|----------------|---------------|--------------------|----------|
| `<ITEM-001>` | `<source>` | `<critical/high/medium>` | `<owner>` | `<sla>` | `<status>` | `<status>` | `<action>` | `<evidence>` |

## 5) Remediation Queue

| item_id | priority | owner | sla | immediate_action | status |
|---------|----------|-------|-----|------------------|--------|
| `<ITEM-001>` | `<priority>` | `<owner>` | `<sla>` | `<action>` | `<status>` |

## 6) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
| `<owner>` | `<n>` | `<n>` | `<n>` | `<n>` | `<window>` |

## 7) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
