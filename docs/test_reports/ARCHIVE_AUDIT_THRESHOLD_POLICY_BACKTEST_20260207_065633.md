# Archive Audit Threshold Policy Backtest & Drift Monitor（Draft）

## 1) Metadata

| field | value |
|------|-------|
| backtest_id | 20260207_065633 |
| generated_at | 2026-02-07 06:56:34 +0800 |
| dashboard_glob | docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD*.md |
| total_runs | 2 |
| operator | codex |

## 2) Threshold Configuration

| threshold | value |
|-----------|-------|
| due_soon_warn_threshold | 1 |
| blocking_high_threshold | 3 |
| checklist_warn_threshold | 1 |
| drift_alert_threshold | 1 |

## 3) Backtest Summary

| metric | value |
|--------|-------|
| critical_runs | 2 |
| high_runs | 0 |
| medium_runs | 0 |
| low_runs | 0 |
| avg_hold_due_soon_total | 1 |
| avg_blocking_reason_total | 3 |
| avg_checklist_readiness_fail | 1 |
| drift_alerts | 0 |
| backtest_status | fail |
| release_guidance | block-policy-rollout-until-high-critical-cleared |

## 4) Per-Run Evaluation

| source | escalation_level | hold_overdue_total | hold_due_soon_total | hold_missing_or_invalid_expiry_total | checklist_fail | checklist_warn | weekly_fail_count | linkage_risk_total | blocking_reason_total |
|--------|------------------|--------------------|---------------------|--------------------------------------|----------------|----------------|-------------------|--------------------|-----------------------|
| docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_20260207_061431.md | critical | 1 | 1 | 1 | 1 | 0 | 1 | 0 | 3 |
| docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md | critical | 1 | 1 | 1 | 1 | 0 | 1 | 0 | 3 |

## 5) Drift Monitor

| metric | first_run | last_run | absolute_diff | drift_status |
|--------|-----------|----------|---------------|--------------|
| hold_due_soon_total | 1 | 1 | 0 | ok |
| blocking_reason_total | 3 | 3 | 0 | ok |
| checklist_readiness_fail | 1 | 1 | 0 | ok |

## 6) Suggested Actions

- immediate:
  - block-policy-rollout-until-high-critical-cleared
- followup:
  - rerun-backtest-after-threshold-adjustment
