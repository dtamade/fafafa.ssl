# Archive Audit Risk Grading & Response Matrix Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| matrix_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| dashboard_report | `<path>` |
| checklist_report | `<path>` |
| hold_review_report | `<path>` |
| weekly_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Risk Snapshot

| metric | value |
|--------|-------|
| risk_score | `<n>` |
| overall_risk | `<low/medium/high/critical>` |
| decision_status | `<pass/warn/fail>` |
| release_advice | `<proceed/proceed-with-mitigation/hold-until-mitigated/block-release>` |
| dashboard_status | `<pass/warn/fail/unknown>` |
| checklist_readiness | `<pass/warn/fail/unknown>` |
| hold_overdue_total | `<n>` |
| hold_due_soon_total | `<n>` |
| hold_missing_or_invalid_expiry_total | `<n>` |
| linkage_risk_total | `<n>` |
| weekly_fail_count | `<n>` |
| blocking_reason_total | `<n>` |

## 3) Risk Grading Rules

| grade | trigger_example | expected_decision |
|-------|-----------------|-------------------|
| critical | overdue/missing-expiry + checklist fail + weekly fail | block-release |
| high | 关键维度 fail 或阻断原因密集（>=3） | hold-until-mitigated |
| medium | 无 fail 但存在 warn/due-soon | proceed-with-mitigation |
| low | 关键输入全 pass 且无阻断原因 | proceed |

## 4) Response Matrix

| risk_item | severity | owner | sla | response_action | evidence |
|-----------|----------|-------|-----|-----------------|----------|
| `<risk_item>` | `<low/medium/high/critical>` | `<owner>` | `<SLA>` | `<action>` | `<evidence>` |

## 5) Blocking Reasons

| reason | status |
|--------|--------|
| `<blocking_reason_or_none>` | `<open/closed/n-a>` |

## 6) Suggested Coordination

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
