# Archive Audit SLA-Rollback Linkage Drill Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| exercise_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| sla_alert_report | `<path>` |
| rollback_report | `<path>` |
| drill_plan_report | `<path>` |
| target_version | `<version>` |
| rollback_version | `<version>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| versioning_status | `<pass/warn/fail/unknown>` |
| rollback_candidates | `<n>` |
| sla_breach_status | `<pass/warn/fail/unknown>` |
| total_alert_items | `<n>` |
| critical_alert_items | `<n>` |
| high_alert_items | `<n>` |

## 3) Linkage Summary

| metric | value |
|--------|-------|
| linkage_items_total | `<n>` |
| matched_rollback_alerts | `<n>` |
| missing_alert_mappings | `<n>` |
| alert_without_rollback | `<n>` |
| critical_linkage_items | `<n>` |
| high_linkage_items | `<n>` |
| medium_linkage_items | `<n>` |
| wave_1_immediate | `<n>` |
| wave_2_short_window | `<n>` |
| wave_3_watchlist | `<n>` |
| owner_hotspots | `<n>` |
| estimated_total_minutes | `<n>` |
| linkage_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Linkage Queue

| step_id | alert_id | rollback_item | priority | owner | target_sla | target_minutes | rollback_version | precheck | rollback_action | verify_action | trigger | status | evidence |
|---------|----------|---------------|----------|-------|------------|----------------|------------------|----------|-----------------|---------------|---------|--------|----------|
| `<SLR-001>` | `<RB-BLK-001>` | `<BLK-001>` | `<critical/high/medium>` | `<owner>` | `<sla>` | `<n>` | `<rollback_version>` | `<precheck>` | `<rollback_action>` | `<verify_action>` | `<trigger>` | `<status>` | `<evidence>` |

## 5) Missing SLA Mappings

| expected_alert_id | rollback_item | rollback_version | rollback_reason | note |
|-------------------|---------------|------------------|-----------------|------|
| `<RB-BLK-XXX>` | `<BLK-XXX>` | `<version>` | `<reason>` | `<note>` |

## 6) Alert Rows Without Rollback Items

| alert_id | owner | target_sla | target_minutes | alert_level | escalation_action | note |
|----------|-------|------------|----------------|-------------|-------------------|------|
| `<RB-BLK-XXX>` | `<owner>` | `<sla>` | `<n>` | `<level>` | `<action>` | `<note>` |

## 7) Escalation Waves

| wave | item_count | target_window |
|------|------------|---------------|
| `wave-1-immediate` | `<n>` | `<1h>` |
| `wave-2-short-window` | `<n>` | `<4h>` |
| `wave-3-watchlist` | `<n>` | `<1bd+>` |

## 8) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
| `<owner>` | `<n>` | `<n>` | `<n>` | `<n>` | `<window>` |

## 9) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
