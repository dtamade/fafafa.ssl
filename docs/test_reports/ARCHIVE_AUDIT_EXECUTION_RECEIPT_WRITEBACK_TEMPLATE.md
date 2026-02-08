# Archive Audit Execution Receipt Writeback Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| writeback_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| execution_receipt_report | `<path>` |
| approval_chain_report | `<path>` |
| retest_gate_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| receipt_execution_readiness | `<pass/warn/fail/unknown>` |
| receipt_release_decision | `<decision>` |
| approval_status | `<pass/warn/fail/unknown>` |
| approval_release_decision | `<decision>` |
| retest_gate_status | `<pass/warn/fail/unknown>` |
| retest_release_advice | `<advice>` |

## 3) Writeback Summary

| metric | value |
|--------|-------|
| total_items | `<n>` |
| retest_signaled_items | `<n>` |
| writeback_changed_items | `<n>` |
| writeback_done_items | `<n>` |
| writeback_waived_items | `<n>` |
| writeback_pending_items | `<n>` |
| writeback_inprogress_items | `<n>` |
| writeback_unknown_items | `<n>` |
| writeback_close_percent | `<n%>` |
| open_critical_items | `<n>` |
| open_high_items | `<n>` |
| writeback_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Writeback Rows

| blocker_code | source | blocker_key | severity | owner | previous_status | retest_status | writeback_status | writeback_note | gate_signal | writeback_evidence |
|--------------|--------|-------------|----------|-------|-----------------|---------------|------------------|----------------|-------------|--------------------|
| `<BLK-001>` | `<source>` | `<key>` | `<severity>` | `<owner>` | `<status>` | `<status>` | `<status>` | `<note>` | `<signal>` | `<evidence>` |

## 5) Receipt Writeback Payload

| blocker_code | writeback_status | writeback_note | writeback_evidence |
|--------------|------------------|----------------|--------------------|
| `<BLK-001>` | `<status>` | `<note>` | `<evidence>` |

## 6) Unresolved After Writeback

| blocker_code | severity | owner | action | writeback_status | gate_signal | evidence |
|--------------|----------|-------|--------|------------------|-------------|----------|
| `<BLK-001>` | `<severity>` | `<owner>` | `<action>` | `<status>` | `<signal>` | `<evidence>` |

## 7) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
