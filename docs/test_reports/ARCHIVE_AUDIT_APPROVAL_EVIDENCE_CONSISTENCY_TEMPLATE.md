# Archive Audit Approval Evidence Consistency Audit Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| audit_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| approval_chain_report | `<path>` |
| retest_gate_report | `<path>` |
| writeback_report | `<path>` |
| convergence_dashboard_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| approval_status | `<pass/warn/fail/unknown>` |
| approval_release_decision | `<decision>` |
| approval_rejected_stages | `<n>` |
| retest_status | `<pass/warn/fail/unknown>` |
| retest_failed | `<n>` |
| retest_release_advice | `<advice>` |
| writeback_status | `<pass/warn/fail/unknown>` |
| writeback_pending_items | `<n>` |
| writeback_close_percent | `<n%>` |
| writeback_release_advice | `<advice>` |
| convergence_status | `<pass/warn/fail/unknown>` |
| convergence_index | `<n%>` |
| convergence_guidance | `<guidance>` |

## 3) Audit Summary

| metric | value |
|--------|-------|
| checks_total | `<n>` |
| checks_pass | `<n>` |
| checks_warn | `<n>` |
| checks_fail | `<n>` |
| audit_status | `<pass/warn/fail>` |
| release_advice | `<advice>` |

## 4) Check Rows

| check_id | area | expected | observed | check_status | note |
|----------|------|----------|----------|--------------|------|
| `<C01>` | `<area>` | `<expected>` | `<observed>` | `<pass/warn/fail>` | `<note>` |

## 5) Mismatch Queue

| check_id | area | severity | note | observed |
|----------|------|----------|------|----------|
| `<Cxx>` | `<area>` | `<warn/fail>` | `<note>` | `<observed>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
