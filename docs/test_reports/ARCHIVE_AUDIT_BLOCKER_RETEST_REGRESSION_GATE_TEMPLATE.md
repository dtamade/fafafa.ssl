# Archive Audit Blocker Retest & Regression Gate Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| gate_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| closure_record_report | `<path>` |
| approval_chain_report | `<path>` |
| retest_pass_blockers | `<csv_or_none>` |
| retest_waive_blockers | `<csv_or_none>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| closure_status | `<pass/warn/fail/unknown>` |
| closure_close_percent | `<n%>` |
| closure_critical_unclosed | `<n>` |
| closure_high_unclosed | `<n>` |
| approval_status | `<pass/warn/fail/unknown>` |
| approval_release_decision | `<decision>` |

## 3) Retest Summary

| metric | value |
|--------|-------|
| total_retest_items | `<n>` |
| retest_passed | `<n>` |
| retest_waived | `<n>` |
| retest_warn | `<n>` |
| retest_failed | `<n>` |
| retest_coverage_percent | `<n%>` |
| open_critical_after_retest | `<n>` |
| open_high_after_retest | `<n>` |
| escalation_open_count | `<n>` |
| regression_gate_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Retest Rows

| blocker_code | severity | owner | action | previous_execution_status | retest_status | gate_signal | evidence |
|--------------|----------|-------|--------|---------------------------|---------------|-------------|----------|
| `<BLK-001>` | `<critical/high/medium/low>` | `<owner>` | `<action>` | `<status>` | `<pass/warn/fail/waived>` | `<signal>` | `<evidence>` |

## 5) Escalation Snapshot

| stage_id | stage_name | stage_status | owner | trigger | required_action |
|----------|------------|--------------|-------|---------|-----------------|
| `<S1>` | `<gate_name>` | `<pass/warn/fail/unknown>` | `<owner>` | `<trigger>` | `<action>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
