# Pre-Release Audit Blockers Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| blocker_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| checklist_report | `<path>` |
| weekly_report | `<path>` |
| risk_matrix_report | `<path>` |
| dashboard_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| checklist_readiness | `<pass/warn/fail/unknown>` |
| checklist_blocking_reasons | `<none_or_reason_list>` |
| weekly_status | `<pass/warn/fail/unknown>` |
| weekly_hold_overdue_total | `<n>` |
| weekly_checklist_fail | `<n>` |
| risk_overall | `<low/medium/high/critical/unknown>` |
| risk_decision_status | `<pass/warn/fail/unknown>` |
| risk_release_advice | `<advice>` |
| dashboard_status | `<pass/warn/fail/unknown>` |
| dashboard_blocking_reason_total | `<n>` |

## 3) Blocker Summary

| metric | value |
|--------|-------|
| blockers_total | `<n>` |
| blockers_critical | `<n>` |
| blockers_high | `<n>` |
| blockers_medium | `<n>` |
| blockers_status | `<pass/warn/fail>` |
| release_gate_decision | `<proceed/proceed-with-mitigation/hold/block-release>` |

## 4) Blocker Items

| blocker_code | source | blocker_key | severity | owner | action | evidence |
|--------------|--------|-------------|----------|-------|--------|----------|
| `<BLK-001>` | `<source>` | `<key>` | `<critical/high/medium>` | `<owner>` | `<action>` | `<evidence>` |

## 5) Extraction Evidence

| check | result |
|-------|--------|
| `<check_name>` | `<pass/fail>` |

## 6) Release Decision

- immediate:
  - `<decision_action_1>`
- followup:
  - `<followup_action_1>`
