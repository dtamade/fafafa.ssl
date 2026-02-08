# Archive Audit Weekly vs Checklist Consistency Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| consistency_id | b33_sample_20260207_0900 |
| generated_at | 2026-02-07 06:32:42 +0800 |
| weekly_report | docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md |
| checklist_report | docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md |
| operator | codex |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| checklist_readiness | fail |
| checklist_blocking_reasons | gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete |
| checklist_hold_overdue | 1 |
| weekly_status | fail |
| weekly_checklist_fail | 1 |
| weekly_checklist_warn | 0 |
| weekly_hold_overdue_total | 1 |
| weekly_linkage_risk_total | 0 |
| weekly_checklist_inputs | 1 |

## 3) Consistency Summary

| metric | value |
|--------|-------|
| total_checks | 7 |
| passed_checks | 7 |
| critical_fail_count | 0 |
| warning_count | 0 |
| consistency_status | pass |
| release_recommendation | consistent-can-proceed |

## 4) Consistency Checks

| check_id | level | result | expected | actual | note |
|----------|-------|--------|----------|--------|------|
| checklist_row_present | critical | pass | weekly checklist aggregate contains checklist source | docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md | source row found |
| readiness_echo | critical | pass | weekly row readiness == checklist readiness | fail vs fail | aligned |
| readiness_count_alignment | critical | pass | weekly checklist_readiness_fail > 0 | fail=1; warn=0; checklist_inputs=1 | aligned |
| blocking_reason_alignment | critical | pass | weekly row blocking_reasons aligns with checklist blocking_reasons | gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete vs gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete | aligned |
| hold_overdue_signal_alignment | warning | pass | if checklist hold_overdue > 0 then weekly hold_overdue_total > 0 | checklist_hold_overdue=1; weekly_hold_overdue_total=1 | aligned |
| weekly_status_guard | critical | pass | if checklist readiness is fail/warn then weekly_status should not be pass | checklist_readiness=fail; weekly_status=fail | aligned |
| weekly_status_rule_consistency | critical | pass | weekly_status should match weekly fail/warn triggering rules | weekly_status=fail; fail=1; hold_overdue=1; linkage_risk=0; warn=0 | aligned |

## 5) Source Row Match

| item | value |
|------|-------|
| weekly_row_found | true |
| weekly_row_source | docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md |
| weekly_row_readiness | fail |
| weekly_row_blocking_reasons | gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete |

## 6) Suggested Actions

- blocking:
  - consistent-can-proceed
- followup:
  - sync-weekly-and-checklist-before-next-cut
