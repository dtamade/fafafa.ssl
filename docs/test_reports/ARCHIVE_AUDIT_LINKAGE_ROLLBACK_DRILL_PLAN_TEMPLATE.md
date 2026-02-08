# Archive Audit Linkage & Rollback Drill Plan Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| drill_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| tracker_report | `<path>` |
| versioning_report | `<path>` |
| anomaly_response_report | `<path>` |
| sla_alert_report | `<path>` |
| target_version | `<version>` |
| rollback_version | `<version>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| tracker_status | `<pass/warn/fail/unknown>` |
| writeback_change_coverage_percent | `<n>%` |
| total_gap_items | `<n>` |
| remediation_queue_items_input | `<n>` |
| critical_gap_items | `<n>` |
| high_gap_items | `<n>` |
| versioning_status | `<pass/warn/fail/unknown>` |
| rollback_candidates | `<n>` |
| anomaly_response_status | `<pass/warn/fail/unknown>` |
| critical_high_open | `<n>` |
| sla_breach_status | `<pass/warn/fail/unknown>` |
| total_alert_items | `<n>` |
| critical_alert_items | `<n>` |
| high_alert_items | `<n>` |

## 3) Drill Summary

| metric | value |
|--------|-------|
| drill_items_total | `<n>` |
| rollback_drill_items | `<n>` |
| critical_steps | `<n>` |
| high_steps | `<n>` |
| medium_steps | `<n>` |
| owner_hotspots | `<n>` |
| estimated_total_minutes | `<n>` |
| drill_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Drill Steps

| step_id | phase | priority | owner | target_sla | estimated_minutes | trigger | action | expected_result | status | evidence |
|---------|-------|----------|-------|------------|-------------------|---------|--------|-----------------|--------|----------|
| `<DRL-001>` | `<phase>` | `<critical/high/medium>` | `<owner>` | `<sla>` | `<n>` | `<trigger>` | `<action>` | `<result>` | `<status>` | `<evidence>` |

## 5) Rollback Exercise Queue

| step_id | priority | owner | precheck | rollback_action | verify_action | status |
|---------|----------|-------|----------|-----------------|---------------|--------|
| `<DRL-001>` | `<priority>` | `<owner>` | `<precheck>` | `<rollback_action>` | `<verify_action>` | `<status>` |

## 6) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
| `<owner>` | `<n>` | `<n>` | `<n>` | `<n>` | `<window>` |

## 7) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
