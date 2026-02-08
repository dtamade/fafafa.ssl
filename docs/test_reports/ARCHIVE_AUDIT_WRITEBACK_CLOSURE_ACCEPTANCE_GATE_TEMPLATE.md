# Archive Audit Writeback Coverage Closure Acceptance Gate Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| gate_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| tracker_report | `<path>` |
| sla_rollback_report | `<path>` |
| versioning_report | `<path>` |
| min_coverage | `<n>%` |
| max_high_gap | `<n>` |
| target_version | `<version>` |
| rollback_version | `<version>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| tracker_status | `<pass/warn/fail/unknown>` |
| writeback_change_coverage_percent | `<n>%` |
| total_gap_items | `<n>` |
| critical_gap_items | `<n>` |
| high_gap_items | `<n>` |
| remediation_queue_items | `<n>` |
| versioning_status | `<pass/warn/fail/unknown>` |
| rollback_candidates | `<n>` |
| linkage_status | `<pass/warn/fail/unknown>` |
| linkage_items_total | `<n>` |
| missing_alert_mappings | `<n>` |
| alert_without_rollback | `<n>` |
| critical_linkage_items | `<n>` |
| high_linkage_items | `<n>` |

## 3) Acceptance Summary

| metric | value |
|--------|-------|
| checks_total | `<n>` |
| checks_passed | `<n>` |
| checks_warn | `<n>` |
| checks_failed | `<n>` |
| critical_failed_checks | `<n>` |
| high_failed_checks | `<n>` |
| outstanding_blockers | `<n>` |
| owner_hotspots | `<n>` |
| acceptance_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Gate Checks

| check_id | severity | observed | threshold | result | remediation_action |
|----------|----------|----------|-----------|--------|--------------------|
| `<gate-coverage-percent>` | `<critical/high/medium>` | `<observed>` | `<threshold>` | `<pass/warn/fail>` | `<action>` |

## 5) Outstanding Blockers

| item_id | priority | owner | sla | status | immediate_action |
|---------|----------|-------|-----|--------|------------------|
| `<WB-BLK-001>` | `<critical/high/medium>` | `<owner>` | `<sla>` | `<status>` | `<action>` |

## 6) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
| `<owner>` | `<n>` | `<n>` | `<n>` | `<n>` | `<window>` |

## 7) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
