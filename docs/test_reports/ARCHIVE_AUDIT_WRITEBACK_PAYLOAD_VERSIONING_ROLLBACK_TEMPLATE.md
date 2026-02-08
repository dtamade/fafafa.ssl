# Archive Audit Writeback Payload Versioning & Rollback Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| version_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| writeback_report | `<path>` |
| linkage_report | `<path>` |
| target_version | `<version>` |
| rollback_version | `<version>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| writeback_status | `<pass/warn/fail/unknown>` |
| writeback_signaled_items | `<n>` |
| writeback_changed_items | `<n>` |
| linkage_status | `<pass/warn/fail/unknown>` |
| mismatch_rows | `<n>` |
| missing_payload_rows | `<n>` |

## 3) Versioning Summary

| metric | value |
|--------|-------|
| total_payload_items | `<n>` |
| done_items | `<n>` |
| waived_items | `<n>` |
| pending_items | `<n>` |
| inprogress_items | `<n>` |
| unknown_items | `<n>` |
| rollback_candidates | `<n>` |
| versioning_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Versioned Payload Rows

| blocker_code | current_status | next_status | target_version | rollback_marker | note | evidence |
|--------------|----------------|-------------|----------------|-----------------|------|----------|
| `<BLK-001>` | `<status>` | `<status>` | `<version>` | `<keep/rollback-candidate>` | `<note>` | `<evidence>` |

## 5) Rollback Queue

| blocker_code | current_status | rollback_version | rollback_reason | note |
|--------------|----------------|------------------|-----------------|------|
| `<BLK-001>` | `<status>` | `<version>` | `<reason>` | `<note>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
