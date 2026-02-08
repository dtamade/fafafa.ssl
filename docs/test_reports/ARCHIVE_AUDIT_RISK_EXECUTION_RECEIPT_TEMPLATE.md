# Archive Audit Risk Response Execution Receipt Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| receipt_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| risk_matrix_report | `<path>` |
| blockers_report | `<path>` |
| threshold_policy_report | `<path>` |
| close_blockers | `<comma_list_or_none>` |
| waive_blockers | `<comma_list_or_none>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| risk_overall | `<low/medium/high/critical/unknown>` |
| risk_decision_status | `<pass/warn/fail/unknown>` |
| risk_release_advice | `<advice>` |
| threshold_escalation_level | `<low/medium/high/critical/unknown>` |
| threshold_decision_status | `<pass/warn/fail/unknown>` |
| threshold_release_policy | `<policy>` |

## 3) Execution Summary

| metric | value |
|--------|-------|
| total_items | `<n>` |
| done_items | `<n>` |
| waived_items | `<n>` |
| inprogress_items | `<n>` |
| pending_items | `<n>` |
| open_critical_items | `<n>` |
| open_high_items | `<n>` |
| completion_percent | `<n%>` |
| execution_readiness | `<pass/warn/fail>` |
| release_decision | `<decision>` |

## 4) Execution Receipt Rows

| blocker_code | source | blocker_key | severity | owner | target_sla | action | execution_status | receipt_note | evidence |
|--------------|--------|-------------|----------|-------|------------|--------|------------------|--------------|----------|
| `<BLK-001>` | `<source>` | `<key>` | `<critical/high/medium/low>` | `<owner>` | `<sla>` | `<action>` | `<done/waived/in-progress/pending>` | `<note>` | `<evidence>` |

## 5) Unresolved Items

| blocker_code | severity | owner | action | execution_status | evidence |
|--------------|----------|-------|--------|------------------|----------|
| `<BLK-xxx_or_none>` | `<severity>` | `<owner>` | `<action>` | `<status>` | `<evidence>` |

## 6) Suggested Follow-up

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
