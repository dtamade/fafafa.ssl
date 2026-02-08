# Archive Audit Weekly Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| week_id | 2026_w05 |
| generated_at | 2026-02-07 05:40:23 +0800 |
| hold_report_inputs | 1 |
| linkage_report_inputs | 1 |
| checklist_report_inputs | 1 |
| operator | codex |

## 2) Weekly Snapshot

| metric | value |
|--------|-------|
| hold_overdue_total | 1 |
| hold_due_soon_total | 1 |
| hold_missing_or_invalid_expiry_total | 1 |
| linkage_risk_total | 0 |
| checklist_readiness_fail | 1 |
| checklist_readiness_warn | 0 |
| weekly_status | fail |

## 3) Hold Aggregate

| source | overdue | due_soon | missing_expiry | invalid_expiry |
|--------|---------|----------|----------------|----------------|
| docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md | 1 | 1 | 1 | 0 |

## 4) Linkage Aggregate

| source | sampled_runs_risk | status |
|--------|-------------------|--------|
| docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md | 0 | pass |

## 5) Checklist Aggregate

| source | readiness | blocking_reasons |
|--------|-----------|------------------|
| docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md | fail | gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete |

## 6) Weekly Actions

- blocking:
  - <blocking_action_1>
- followup:
  - <followup_action_1>

> blocking_reason_count: 3
