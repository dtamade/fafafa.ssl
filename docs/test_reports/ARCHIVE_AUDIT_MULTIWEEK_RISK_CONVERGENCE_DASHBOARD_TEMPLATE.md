# Archive Audit Multi-Week Risk Convergence Dashboard Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| dashboard_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| backtest_glob | `<glob>` |
| approval_chain_glob | `<glob>` |
| retest_gate_glob | `<glob>` |
| backtest_files | `<n>` |
| approval_chain_files | `<n>` |
| retest_gate_files | `<n>` |
| operator | `<name_or_ci_job>` |

## 2) Latest Snapshot

| metric | value |
|--------|-------|
| latest_backtest_status | `<pass/warn/fail/unknown>` |
| latest_backtest_file | `<path_or_none>` |
| latest_approval_status | `<pass/warn/fail/unknown>` |
| latest_approval_file | `<path_or_none>` |
| latest_retest_status | `<pass/warn/fail/unknown>` |
| latest_retest_file | `<path_or_none>` |

## 3) Convergence Summary

| metric | value |
|--------|-------|
| total_score | `<n>` |
| max_score | `<n>` |
| convergence_index | `<n%>` |
| trend_alerts | `<n>` |
| insufficient_signals | `<n>` |
| risk_convergence_status | `<pass/warn/fail>` |
| release_guidance | `<guidance>` |

## 4) Stream Snapshot

| stream | files | pass | warn | fail | unknown | latest_status |
|--------|-------|------|------|------|---------|---------------|
| `<stream>` | `<n>` | `<n>` | `<n>` | `<n>` | `<n>` | `<status>` |

## 5) Per-Run Snapshot

| stream | source | primary_metric | secondary_metric | status | score |
|--------|--------|----------------|------------------|--------|-------|
| `<stream>` | `<file>` | `<n>` | `<n>` | `<status>` | `<n>` |

## 6) Trend Signals

| metric | first_value | last_value | absolute_diff | trend_direction | signal_status |
|--------|-------------|------------|---------------|-----------------|---------------|
| `<metric>` | `<n>` | `<n>` | `<n>` | `<up/down/flat>` | `<stable/improving/alert/insufficient-window>` |

## 7) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
