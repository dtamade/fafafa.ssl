# Archive Audit Retest-Approval Writeback Linkage Validation（Draft）

## 1) Metadata

| field | value |
|------|-------|
| linkage_id | 20260207_081719 |
| generated_at | 2026-02-07 08:17:19 +0800 |
| retest_gate_report | docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md |
| approval_chain_report | docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md |
| writeback_report | docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md |
| operator | codex |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| approval_status | fail |
| retest_gate_status | fail |
| writeback_status | fail |
| writeback_signaled_items | 14 |
| writeback_changed_items | 0 |

## 3) Linkage Summary

| metric | value |
|--------|-------|
| total_rows | 14 |
| matched_rows | 14 |
| mismatch_rows | 0 |
| missing_payload_rows | 0 |
| gate_alignment_status | pass |
| gate_alignment_note | approval/retest/writeback status aligned |
| linkage_status | fail |
| release_advice | block-release-and-force-writeback-change |

## 4) Linkage Rows

| blocker_code | retest_status | expected_writeback_status | actual_writeback_status | row_status | note | gate_signal | evidence |
|--------------|---------------|---------------------------|-------------------------|------------|------|-------------|----------|
| BLK-001 | fail | pending | pending | pass | mapped-consistently | severity-critical-still-open | readiness=fail |
| BLK-002 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | blocking_reason=gate_has_unknown_or_missing |
| BLK-003 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | blocking_reason=hold_overdue_exists |
| BLK-004 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | blocking_reason=hold_expiry_metadata_incomplete |
| BLK-005 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | weekly_status=fail |
| BLK-006 | fail | pending | pending | pass | mapped-consistently | severity-critical-still-open | hold_overdue_total=1 |
| BLK-007 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | checklist_readiness_fail=1 |
| BLK-008 | fail | pending | pending | pass | mapped-consistently | severity-critical-still-open | overall_risk=critical; decision_status=fail |
| BLK-009 | fail | pending | pending | pass | mapped-consistently | severity-critical-still-open | readiness=fail; checklist_status=fail |
| BLK-010 | fail | pending | pending | pass | mapped-consistently | severity-critical-still-open | overdue=1/1; due_soon=1/1 |
| BLK-011 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | weekly_status=fail/fail; weekly_fail_count=1 |
| BLK-012 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | blocking_reason_total=3 |
| BLK-013 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | dashboard_status=fail |
| BLK-014 | fail | pending | pending | pass | mapped-consistently | severity-high-still-open | blocking_reason_total=3 |

## 5) Mismatch Queue

| blocker_code | expected_writeback | actual_writeback | reason | evidence |
|--------------|--------------------|------------------|--------|----------|
| none | n/a | n/a | no-mismatch | n/a |

## 6) Suggested Actions

- immediate:
  - block-release-and-force-writeback-change
- followup:
  - regenerate-writeback-payload-after-retest-sync
