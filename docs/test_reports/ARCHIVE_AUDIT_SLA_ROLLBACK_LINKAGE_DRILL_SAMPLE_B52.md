# Archive Audit SLA-Rollback Linkage Drill（Draft）

## 1) Metadata

| field | value |
|------|-------|
| exercise_id | b52_sample_20260207_1930 |
| generated_at | 2026-02-07 11:25:43 +0800 |
| sla_alert_report | docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md |
| rollback_report | docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md |
| drill_plan_report | docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md |
| target_version | wbv-b46-sample |
| rollback_version | wbv-b45-prev |
| operator | codex |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| versioning_status | fail |
| rollback_candidates | 14 |
| sla_breach_status | fail |
| total_alert_items | 21 |
| critical_alert_items | 5 |
| high_alert_items | 16 |

## 3) Linkage Summary

| metric | value |
|--------|-------|
| linkage_items_total | 14 |
| matched_rollback_alerts | 14 |
| missing_alert_mappings | 0 |
| alert_without_rollback | 0 |
| critical_linkage_items | 0 |
| high_linkage_items | 14 |
| medium_linkage_items | 0 |
| wave_1_immediate | 0 |
| wave_2_short_window | 14 |
| wave_3_watchlist | 0 |
| owner_hotspots | 1 |
| estimated_total_minutes | 1680 |
| linkage_status | fail |
| release_advice | block-release-and-open-sla-rollback-war-room |

## 4) Linkage Queue

| step_id | alert_id | rollback_item | priority | owner | target_sla | target_minutes | rollback_version | precheck | rollback_action | verify_action | trigger | status | evidence |
|---------|----------|---------------|----------|-------|------------|----------------|------------------|----------|-----------------|---------------|---------|--------|----------|
| SLR-001 | RB-BLK-001 | BLK-001 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-001 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-002 | RB-BLK-002 | BLK-002 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-002 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-003 | RB-BLK-003 | BLK-003 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-003 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-004 | RB-BLK-004 | BLK-004 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-004 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-005 | RB-BLK-005 | BLK-005 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-005 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-006 | RB-BLK-006 | BLK-006 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-006 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-007 | RB-BLK-007 | BLK-007 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-007 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-008 | RB-BLK-008 | BLK-008 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-008 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-009 | RB-BLK-009 | BLK-009 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-009 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-010 | RB-BLK-010 | BLK-010 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-010 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-011 | RB-BLK-011 | BLK-011 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-011 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-012 | RB-BLK-012 | BLK-012 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-012 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-013 | RB-BLK-013 | BLK-013 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-013 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |
| SLR-014 | RB-BLK-014 | BLK-014 | high | release-manager | 4h | 240 | wbv-b45-prev | confirm-change-freeze-and-payload-snapshot | execute-rollback-wbv-b45-prev-for-BLK-014 | rerun-versioning-and-sla-alert-checks | status=pending; alert=breach-risk-medium | planned | alert_source=anomaly_response; observed=status=queued; severity=high; rollback_reason=rollback-candidate; note=retest-fail-keep-open |

## 5) Missing SLA Mappings

| expected_alert_id | rollback_item | rollback_version | rollback_reason | note |
|-------------------|---------------|------------------|-----------------|------|
| none | n/a | n/a | n/a | all rollback items linked to SLA alerts |

## 6) Alert Rows Without Rollback Items

| alert_id | owner | target_sla | target_minutes | alert_level | escalation_action | note |
|----------|-------|------------|----------------|-------------|-------------------|------|
| none | n/a | n/a | 0 | ok | n/a | all rollback-related alerts linked |

## 7) Escalation Waves

| wave | item_count | target_window |
|------|------------|---------------|
| wave-1-immediate | 0 | <1h |
| wave-2-short-window | 14 | 4h |
| wave-3-watchlist | 0 | 1bd+ |

## 8) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
| release-manager | 0 | 14 | 0 | 14 | 4h |

## 9) Suggested Actions

- immediate:
  - block-release-and-open-sla-rollback-war-room
- followup:
  - rerun-sla-rollback-linkage-drill-after-mapping-closure
  - rerun-b48-sla-alert-and-b46-versioning-validation
