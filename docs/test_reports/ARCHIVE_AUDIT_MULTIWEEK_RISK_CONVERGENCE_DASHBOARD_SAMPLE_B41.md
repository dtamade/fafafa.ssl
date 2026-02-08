# Archive Audit Multi-Week Risk Convergence Dashboard（Draft）

## 1) Metadata

| field | value |
|------|-------|
| dashboard_id | b41_sample_20260207_1300 |
| generated_at | 2026-02-07 07:48:49 +0800 |
| backtest_glob | docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST*.md |
| approval_chain_glob | docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN*.md |
| retest_gate_glob | docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE*.md |
| backtest_files | 2 |
| approval_chain_files | 2 |
| retest_gate_files | 2 |
| operator | codex |

## 2) Latest Snapshot

| metric | value |
|--------|-------|
| latest_backtest_status | fail |
| latest_backtest_file | docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md |
| latest_approval_status | fail |
| latest_approval_file | docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md |
| latest_retest_status | fail |
| latest_retest_file | docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md |

## 3) Convergence Summary

| metric | value |
|--------|-------|
| total_score | 0 |
| max_score | 12 |
| convergence_index | 0% |
| trend_alerts | 0 |
| insufficient_signals | 0 |
| risk_convergence_status | fail |
| release_guidance | block-release-until-risk-converges |

## 4) Stream Snapshot

| stream | files | pass | warn | fail | unknown | latest_status |
|--------|-------|------|------|------|---------|---------------|
| backtest | 2 | 0 | 0 | 2 | 0 | fail |
| approval_chain | 2 | 0 | 0 | 2 | 0 | fail |
| retest_gate | 2 | 0 | 0 | 2 | 0 | fail |

## 5) Per-Run Snapshot

| stream | source | primary_metric | secondary_metric | status | score |
|--------|--------|----------------|------------------|--------|-------|
| backtest | docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_20260207_065633.md | 2 | 0 | fail | 0 |
| backtest | docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md | 2 | 0 | fail | 0 |
| approval_chain | docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_20260207_073404.md | 4 | 0 | fail | 0 |
| approval_chain | docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md | 4 | 0 | fail | 0 |
| retest_gate | docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_20260207_074305.md | 14 | 5 | fail | 0 |
| retest_gate | docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md | 14 | 5 | fail | 0 |

## 6) Trend Signals

| metric | first_value | last_value | absolute_diff | trend_direction | signal_status |
|--------|-------------|------------|---------------|-----------------|---------------|
| backtest_critical_runs | 2 | 2 | 0 | flat | stable |
| approval_rejected_stages | 4 | 4 | 0 | flat | stable |
| retest_failed_items | 14 | 14 | 0 | flat | stable |

## 7) Suggested Actions

- immediate:
  - block-release-until-risk-converges
- followup:
  - refresh-dashboard-after-next-weekly-cycle
