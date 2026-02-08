# Pre-Release Audit Blockers（Draft）

## 1) Metadata

| field | value |
|------|-------|
| blocker_id | 20260207_062446 |
| generated_at | 2026-02-07 06:24:47 +0800 |
| checklist_report | docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md |
| weekly_report | docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md |
| risk_matrix_report | docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md |
| dashboard_report | docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md |
| operator | codex |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| checklist_readiness | fail |
| checklist_blocking_reasons | gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete |
| weekly_status | fail |
| weekly_hold_overdue_total | 1 |
| weekly_checklist_fail | 1 |
| risk_overall | critical |
| risk_decision_status | fail |
| risk_release_advice | block-release |
| dashboard_status | fail |
| dashboard_blocking_reason_total | 3 |

## 3) Blocker Summary

| metric | value |
|--------|-------|
| blockers_total | 14 |
| blockers_critical | 5 |
| blockers_high | 9 |
| blockers_medium | 0 |
| blockers_status | fail |
| release_gate_decision | block-release |

## 4) Blocker Items

| blocker_code | source | blocker_key | severity | owner | action | evidence |
|--------------|--------|-------------|----------|-------|--------|----------|
| BLK-001 | checklist | checklist_readiness_fail | critical | release-manager | block-release-and-close-checklist-gaps | readiness=fail |
| BLK-002 | checklist | gate_has_unknown_or_missing | high | qa-secops | resolve-blocking-reason | blocking_reason=gate_has_unknown_or_missing |
| BLK-003 | checklist | hold_overdue_exists | high | qa-secops | resolve-blocking-reason | blocking_reason=hold_overdue_exists |
| BLK-004 | checklist | hold_expiry_metadata_incomplete | high | qa-secops | resolve-blocking-reason | blocking_reason=hold_expiry_metadata_incomplete |
| BLK-005 | weekly | weekly_status_fail | high | release-ops | stabilize-weekly-signals | weekly_status=fail |
| BLK-006 | weekly | hold_overdue_present | critical | qa-secops | clear-overdue-hold-items | hold_overdue_total=1 |
| BLK-007 | weekly | checklist_fail_present | high | release-manager | close-checklist-failures | checklist_readiness_fail=1 |
| BLK-008 | risk_matrix | risk_decision_fail | critical | risk-owner | execute-risk-response-before-release | overall_risk=critical; decision_status=fail |
| BLK-009 | risk_matrix_response | release_checklist | critical | release-manager | block-release-and-escalate | readiness=fail; checklist_status=fail |
| BLK-010 | risk_matrix_response | hold_expiry_control | critical | qa-secops | block-release-and-escalate | overdue=1/1; due_soon=1/1 |
| BLK-011 | risk_matrix_response | weekly_execution_signal | high | release-ops | open-incident-and-fix-before-cut | weekly_status=fail/fail; weekly_fail_count=1 |
| BLK-012 | risk_matrix_response | blocking_reason_density | high | release-manager | open-incident-and-fix-before-cut | blocking_reason_total=3 |
| BLK-013 | dashboard | dashboard_status_fail | high | release-manager | clear-dashboard-fail-signals | dashboard_status=fail |
| BLK-014 | dashboard | dashboard_blocking_reasons_present | high | release-manager | reduce-dashboard-blocking-reasons | blocking_reason_total=3 |

## 5) Extraction Evidence

| check | result |
|-------|--------|
| checklist_report_readable | pass |
| weekly_report_readable | pass |
| risk_matrix_report_readable | pass |
| dashboard_report_readable | pass |

## 6) Release Decision

- immediate:
  - block-release
- followup:
  - sync-blockers-with-release-owner
