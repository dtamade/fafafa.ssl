# Archive Audit Closure Acceptance Retry Report

## Metadata

| Field | Value |
|-------|-------|
| retry_id | b56_sample_20260207_2056 |
| generated_at | 2026-02-07 21:00:09 +0800 |
| mode | dry-run |
| closure_gate_report | docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md |
| autofix_report | docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md |
| verify_report | n/a |
| max_retries | 3 |
| retry_delay | 5s |
| escalate_threshold | 2 |

## Summary

| Metric | Value |
|--------|-------|
| total_items | 76 |
| retry_items | 76 |
| escalate_items | 0 |
| skip_items | 0 |
| pending_items | 76 |
| retry_status | pending |

## Retry Actions

| item_id | priority | owner | status | retry_count | action |
|---------|----------|-------|--------|-------------|--------|
| tracker_status | fail |  | retry-pending | 1 | retry |
| versioning_status | fail |  | retry-pending | 1 | retry |
| linkage_status | fail |  | retry-pending | 1 | retry |
| acceptance_status | fail |  | retry-pending | 1 | retry |
| gate-tracker-status | critical | fail | retry-pending | 1 | retry |
| gate-coverage-percent | critical | 0% | retry-pending | 1 | retry |
| gate-total-gap-items | critical | 48 | retry-pending | 1 | retry |
| gate-critical-gap-items | critical | 8 | retry-pending | 1 | retry |
| gate-high-gap-items | high | 40 | retry-pending | 1 | retry |
| gate-versioning-status | critical | fail | retry-pending | 1 | retry |
| gate-rollback-candidates | critical | 14 | retry-pending | 1 | retry |
| gate-linkage-status | critical | fail | retry-pending | 1 | retry |
| WB-BLK-001 | critical | release-manager | retry-pending | 1 | retry |
| WB-BLK-002 | high | qa-secops | retry-pending | 1 | retry |
| WB-BLK-003 | high | qa-secops | retry-pending | 1 | retry |
| WB-BLK-004 | high | qa-secops | retry-pending | 1 | retry |
| WB-BLK-005 | high | release-ops | retry-pending | 1 | retry |
| WB-BLK-006 | critical | qa-secops | retry-pending | 1 | retry |
| WB-BLK-007 | high | release-manager | retry-pending | 1 | retry |
| WB-BLK-008 | critical | risk-owner | retry-pending | 1 | retry |
| WB-BLK-009 | critical | release-manager | retry-pending | 1 | retry |
| WB-BLK-010 | critical | qa-secops | retry-pending | 1 | retry |
| WB-BLK-011 | high | release-ops | retry-pending | 1 | retry |
| WB-BLK-012 | high | release-manager | retry-pending | 1 | retry |
| WB-BLK-013 | high | release-manager | retry-pending | 1 | retry |
| WB-BLK-014 | high | release-manager | retry-pending | 1 | retry |
| POL-writeback-change-coverage | critical | release-manager | retry-pending | 1 | retry |
| autofix_status | pending |  | retry-pending | 1 | retry |
| WB-BLK-001 | critical | release-manager | retry-pending | 1 | retry |
| WB-BLK-002 | high | qa-secops | retry-pending | 1 | retry |
| WB-BLK-003 | high | qa-secops | retry-pending | 1 | retry |
| WB-BLK-004 | high | qa-secops | retry-pending | 1 | retry |
| WB-BLK-005 | high | release-ops | retry-pending | 1 | retry |
| WB-BLK-006 | critical | qa-secops | retry-pending | 1 | retry |
| WB-BLK-007 | high | release-manager | retry-pending | 1 | retry |
| WB-BLK-008 | critical | risk-owner | retry-pending | 1 | retry |
| WB-BLK-009 | critical | release-manager | retry-pending | 1 | retry |
| WB-BLK-010 | critical | qa-secops | retry-pending | 1 | retry |
| WB-BLK-011 | high | release-ops | retry-pending | 1 | retry |
| WB-BLK-012 | high | release-manager | retry-pending | 1 | retry |
| WB-BLK-013 | high | release-manager | retry-pending | 1 | retry |
| WB-BLK-014 | high | release-manager | retry-pending | 1 | retry |
| POL-writeback-change-coverage | critical | release-manager | retry-pending | 1 | retry |
| AUD-C06 | critical | release-manager+secops | retry-pending | 1 | retry |
| AUD-C08 | high | release-manager | retry-pending | 1 | retry |
| POL-writeback-change-coverage | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-001 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-002 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-003 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-004 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-005 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-006 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-007 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-008 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-009 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-010 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-011 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-012 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-013 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-014 | high | release-manager | retry-pending | 1 | retry |
| AUD-C06 | critical | release-manager+secops | retry-pending | 1 | retry |
| POL-writeback-change-coverage | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-001 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-002 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-003 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-004 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-005 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-006 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-007 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-008 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-009 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-010 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-011 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-012 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-013 | high | release-manager | retry-pending | 1 | retry |
| RB-BLK-014 | high | release-manager | retry-pending | 1 | retry |

## Escalation Queue

| item_id | priority | owner | reason |
|---------|----------|-------|--------|

## Next Steps

- 76 items pending retry.
- Re-run with --apply to execute retries.

## Release Advice

| Condition | Advice |
|-----------|--------|
| retry_status=pass | proceed-to-release |
| retry_status=pending | re-run-with-apply |
| retry_status=escalate | manual-intervention-required |
