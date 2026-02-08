# Archive Audit Dashboard Threshold Policy Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| policy_id | 20260207_063805 |
| generated_at | 2026-02-07 06:38:05 +0800 |
| dashboard_report | docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md |
| operator | codex |

## 2) Threshold Configuration

| threshold | value |
|-----------|-------|
| due_soon_warn_threshold | 1 |
| blocking_high_threshold | 3 |
| checklist_warn_threshold | 1 |

## 3) Metric Evaluation

| metric | value | threshold_rule | severity | owner | sla | action |
|--------|-------|----------------|----------|-------|-----|--------|
| dashboard_status | fail | fail/unknown=>critical; warn=>high | critical | release-manager+secops | <1h | immediate-escalation-and-release-block |
| hold_overdue_total | 1 | >0=>critical | critical | release-manager+secops | <1h | immediate-escalation-and-release-block |
| hold_missing_or_invalid_expiry_total | 1 | >0=>critical | critical | release-manager+secops | <1h | immediate-escalation-and-release-block |
| linkage_risk_total | 0 | >0=>critical | low | audit-owner | next-weekly | monitor-only |
| checklist_readiness_fail | 1 | >0=>critical | critical | release-manager+secops | <1h | immediate-escalation-and-release-block |
| weekly_fail_count | 1 | >0=>critical | critical | release-manager+secops | <1h | immediate-escalation-and-release-block |
| blocking_reason_total | 3 | <3=>medium; >=3=>high | high | release-manager | 4h | open-incident-and-clear-before-cut |
| hold_due_soon_total | 1 | >=1=>medium | medium | qa-owner | 1bd | track-and-mitigate-with-owner |
| checklist_readiness_warn_or_unknown | 0 | >=1=>medium | low | audit-owner | next-weekly | monitor-only |
| weekly_warn_or_unknown_count | 0 | >0=>medium | low | audit-owner | next-weekly | monitor-only |

## 4) Escalation Summary

| metric | value |
|--------|-------|
| escalation_level | critical |
| decision_status | fail |
| release_policy | block-release |
| critical_signals | 5 |
| high_signals | 1 |
| medium_signals | 1 |

## 5) Suggested Escalation Runbook

- immediate:
  - immediate-escalation-and-release-block
- followup:
  - align-threshold-policy-with-release-board
