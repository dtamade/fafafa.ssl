# Archive Audit Execution Approval Chain Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| chain_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| execution_receipt_report | `<path>` |
| closure_record_report | `<path>` |
| remediation_plan_report | `<path>` |
| backtest_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| execution_readiness | `<pass/warn/fail/unknown>` |
| execution_release_decision | `<decision>` |
| closure_status | `<pass/warn/fail/unknown>` |
| closure_release_advice | `<advice>` |
| remediation_status | `<pass/warn/fail/unknown>` |
| remediation_release_guidance | `<guidance>` |
| backtest_status | `<pass/warn/fail/unknown>` |
| backtest_release_guidance | `<guidance>` |

## 3) Approval Summary

| metric | value |
|--------|-------|
| total_stages | `<n>` |
| approved_stages | `<n>` |
| conditional_stages | `<n>` |
| rejected_stages | `<n>` |
| pending_review_stages | `<n>` |
| approval_status | `<pass/warn/fail>` |
| release_decision | `<decision>` |

## 4) Approval Chain Rows

| stage_id | stage_name | source_report | gate_metric | gate_value | stage_status | approver_role | target_sla | approval_note | followup_action |
|----------|------------|---------------|-------------|------------|--------------|---------------|------------|---------------|-----------------|
| `<S1>` | `<gate_name>` | `<report_path>` | `<metric>` | `<value>` | `<pass/warn/fail/unknown>` | `<owner>` | `<sla>` | `<note>` | `<action>` |

## 5) Escalation Queue

| stage_id | stage_name | stage_status | owner | trigger | required_action |
|----------|------------|--------------|-------|---------|-----------------|
| `<Sx>` | `<gate_name>` | `<warn/fail/unknown>` | `<owner>` | `<trigger>` | `<required_action>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
