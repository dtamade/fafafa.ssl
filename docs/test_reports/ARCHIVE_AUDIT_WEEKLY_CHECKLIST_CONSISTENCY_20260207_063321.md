# Archive Audit Weekly vs Checklist Consistency Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| consistency_id | 20260207_063321 |
| generated_at | 2026-02-07 06:33:21 +0800 |
| weekly_report | docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md |
| checklist_report | /tmp/tmp.ZHmjtNCn07 |
| operator | codex |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| checklist_readiness | pass |
| checklist_blocking_reasons | none |
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
| passed_checks | 3 |
| critical_fail_count | 4 |
| warning_count | 0 |
| consistency_status | fail |
| release_recommendation | block-until-weekly-checklist-aligned |

## 4) Consistency Checks

| check_id | level | result | expected | actual | note |
|----------|-------|--------|----------|--------|------|
| checklist_row_present | critical | fail | weekly checklist aggregate contains checklist source | not found | source row missing |
| readiness_echo | critical | fail | weekly row readiness == checklist readiness | n/a vs pass | readiness mismatch |
| readiness_count_alignment | critical | fail | weekly checklist_readiness_fail == 0 and warn == 0 | fail=1; warn=0; checklist_inputs=1 | single-checklist scope should not have fail/warn counts |
| blocking_reason_alignment | critical | fail | weekly row blocking_reasons aligns with checklist blocking_reasons | none vs none | cannot compare without checklist row |
| hold_overdue_signal_alignment | warning | pass | if checklist hold_overdue > 0 then weekly hold_overdue_total > 0 | checklist_hold_overdue=1; weekly_hold_overdue_total=1 | aligned |
| weekly_status_guard | critical | pass | if checklist readiness is fail/warn then weekly_status should not be pass | checklist_readiness=pass; weekly_status=fail | aligned |
| weekly_status_rule_consistency | critical | pass | weekly_status should match weekly fail/warn triggering rules | weekly_status=fail; fail=1; hold_overdue=1; linkage_risk=0; warn=0 | aligned |

## 5) Source Row Match

| item | value |
|------|-------|
| weekly_row_found | false |
| weekly_row_source | n/a |
| weekly_row_readiness | n/a |
| weekly_row_blocking_reasons | none |

## 6) Suggested Actions

- blocking:
  - block-until-weekly-checklist-aligned
- followup:
  - sync-weekly-and-checklist-before-next-cut
