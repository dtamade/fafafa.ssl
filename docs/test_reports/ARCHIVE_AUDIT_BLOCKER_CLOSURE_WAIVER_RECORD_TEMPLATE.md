# Archive Audit Blocker Closure & Waiver Record Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| record_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| execution_receipt_report | `<path>` |
| required_close_percent | `<n>` |
| waiver_reason_default | `<text>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| input_execution_readiness | `<pass/warn/fail/unknown>` |
| input_release_decision | `<decision>` |
| total_items | `<n>` |
| done_items | `<n>` |
| waived_items | `<n>` |
| pending_items | `<n>` |
| inprogress_items | `<n>` |
| unknown_items | `<n>` |

## 3) Closure Summary

| metric | value |
|--------|-------|
| close_percent | `<n%>` |
| closure_status | `<pass/warn/fail>` |
| critical_unclosed | `<n>` |
| high_unclosed | `<n>` |
| release_advice | `<advice>` |

## 4) Closure Verification Rows

| blocker_code | source | blocker_key | severity | owner | target_sla | action | execution_status | closure_check | closure_note | receipt_note | evidence |
|--------------|--------|-------------|----------|-------|------------|--------|------------------|---------------|--------------|-------------|----------|
| `<BLK-001>` | `<source>` | `<key>` | `<critical/high/medium/low>` | `<owner>` | `<sla>` | `<action>` | `<done/waived/in-progress/pending>` | `<pass/warn/fail>` | `<note>` | `<receipt_note>` | `<evidence>` |

## 5) Waiver Records

| blocker_code | severity | owner | action | waiver_reason | evidence |
|--------------|----------|-------|--------|---------------|----------|
| `<BLK-xxx_or_none>` | `<severity>` | `<owner>` | `<action>` | `<reason>` | `<evidence>` |

## 6) Unclosed Items

| blocker_code | severity | owner | action | execution_status | evidence |
|--------------|----------|-------|--------|------------------|----------|
| `<BLK-xxx_or_none>` | `<severity>` | `<owner>` | `<action>` | `<status>` | `<evidence>` |

## 7) Suggested Actions

- blocking:
  - `<blocking_action_1>`
- followup:
  - `<followup_action_1>`
