# Archive Audit Status Dashboard（Draft）

## 1) Metadata

| field | value |
|------|-------|
| dashboard_id | b30_sample_20260207_0730 |
| generated_at | 2026-02-07 06:14:31 +0800 |
| hold_report_inputs | 1 |
| linkage_report_inputs | 1 |
| checklist_report_inputs | 1 |
| weekly_report_inputs | 1 |
| operator | codex |

## 2) Dashboard Snapshot

| metric | value |
|--------|-------|
| dashboard_status | fail |
| hold_status | fail |
| linkage_status | pass |
| checklist_status | fail |
| weekly_status | fail |
| hold_overdue_total | 1 |
| hold_due_soon_total | 1 |
| hold_missing_or_invalid_expiry_total | 1 |
| linkage_risk_total | 0 |
| checklist_readiness_fail | 1 |
| checklist_readiness_warn_or_unknown | 0 |
| weekly_fail_count | 1 |
| weekly_warn_or_unknown_count | 0 |
| blocking_reason_total | 3 |

## 3) Signal Board

| dimension | status | key_metrics | evidence |
|-----------|--------|-------------|----------|
| hold_expiry | fail | overdue=1; due_soon=1; missing_or_invalid=1 | docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md |
| audit_linkage | pass | sampled_runs_risk_total=0 | docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md |
| release_checklist | fail | fail=1; warn_or_unknown=0 | docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md |
| weekly_execution | fail | fail=1; warn_or_unknown=0 | docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md |
| overall_dashboard | fail | inputs=4; blocking_reasons=3 | docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md |

## 4) Hold Aggregate Detail

| source | overdue | due_soon | missing_expiry | invalid_expiry | row_status |
|--------|---------|----------|----------------|----------------|------------|
| docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md | 1 | 1 | 1 | 0 | fail |

## 5) Linkage Aggregate Detail

| source | sampled_runs_risk | source_status | row_status |
|--------|-------------------|---------------|------------|
| docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md | 0 | pass | pass |

## 6) Checklist Aggregate Detail

| source | readiness | blocking_reasons |
|--------|-----------|------------------|
| docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md | fail | gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete |

## 7) Weekly Trend

| source | weekly_status | hold_overdue_total | checklist_readiness_fail |
|--------|---------------|--------------------|--------------------------|
| docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md | fail | 1 | 1 |

## 8) Blocking Reason Aggregate

| reason | count |
|--------|-------|
| gate_has_unknown_or_missing | 1 |
| hold_expiry_metadata_incomplete | 1 |
| hold_overdue_exists | 1 |

## 9) Suggested Actions

- blocking:
  - pause-release-and-clear-blockers
- followup:
  - review-hold-and-checklist-within-24h
