# Archive Audit Retest-Approval Writeback Linkage Validation Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| linkage_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| retest_gate_report | `<path>` |
| approval_chain_report | `<path>` |
| writeback_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| approval_status | `<pass/warn/fail/unknown>` |
| retest_gate_status | `<pass/warn/fail/unknown>` |
| writeback_status | `<pass/warn/fail/unknown>` |
| writeback_signaled_items | `<n>` |
| writeback_changed_items | `<n>` |

## 3) Linkage Summary

| metric | value |
|--------|-------|
| total_rows | `<n>` |
| matched_rows | `<n>` |
| mismatch_rows | `<n>` |
| missing_payload_rows | `<n>` |
| gate_alignment_status | `<pass/warn/fail>` |
| gate_alignment_note | `<note>` |
| linkage_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Linkage Rows

| blocker_code | retest_status | expected_writeback_status | actual_writeback_status | row_status | note | gate_signal | evidence |
|--------------|---------------|---------------------------|-------------------------|------------|------|-------------|----------|
| `<BLK-001>` | `<status>` | `<status>` | `<status>` | `<pass/warn/fail>` | `<note>` | `<signal>` | `<evidence>` |

## 5) Mismatch Queue

| blocker_code | expected_writeback | actual_writeback | reason | evidence |
|--------------|--------------------|------------------|--------|----------|
| `<BLK-001>` | `<status>` | `<status>` | `<reason>` | `<evidence>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
