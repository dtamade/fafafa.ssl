# Archive Audit Risk Grading & Response Matrix（Draft）

## 1) Metadata

| field | value |
|------|-------|
| matrix_id | 20260207_061943 |
| generated_at | 2026-02-07 06:19:43 +0800 |
| dashboard_report | docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md |
| checklist_report | docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md |
| hold_review_report | docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md |
| weekly_report | docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md |
| operator | codex |

## 2) Risk Snapshot

| metric | value |
|--------|-------|
| risk_score | 21 |
| overall_risk | critical |
| decision_status | fail |
| release_advice | block-release |
| dashboard_status | fail |
| checklist_readiness | fail |
| hold_overdue_total | 1 |
| hold_due_soon_total | 1 |
| hold_missing_or_invalid_expiry_total | 1 |
| linkage_risk_total | 0 |
| weekly_fail_count | 1 |
| blocking_reason_total | 3 |

## 3) Risk Grading Rules

| grade | trigger_example | expected_decision |
|-------|-----------------|-------------------|
| critical | overdue/missing-expiry + checklist fail + weekly fail | block-release |
| high | 关键维度 fail 或阻断原因密集（>=3） | hold-until-mitigated |
| medium | 无 fail 但存在 warn/due-soon | proceed-with-mitigation |
| low | 关键输入全 pass 且无阻断原因 | proceed |

## 4) Response Matrix

| risk_item | severity | owner | sla | response_action | evidence |
|-----------|----------|-------|-----|-----------------|----------|
| release_checklist | critical | release-manager | <1h | block-release-and-escalate | readiness=fail; checklist_status=fail |
| hold_expiry_control | critical | qa-secops | <1h | block-release-and-escalate | overdue=1/1; due_soon=1/1 |
| audit_linkage_signal | low | audit-owner | next-weekly | monitor-in-routine | linkage_risk_total=0; linkage_status=pass |
| weekly_execution_signal | high | release-ops | 4h | open-incident-and-fix-before-cut | weekly_status=fail/fail; weekly_fail_count=1 |
| blocking_reason_density | high | release-manager | 4h | open-incident-and-fix-before-cut | blocking_reason_total=3 |

## 5) Blocking Reasons

| reason | status |
|--------|--------|
| gate_has_unknown_or_missing | open |
| hold_overdue_exists | open |
| hold_expiry_metadata_incomplete | open |

## 6) Suggested Coordination

- immediate:
  - block-release-and-escalate
- followup:
  - align-risk-owner-and-update-dashboard
