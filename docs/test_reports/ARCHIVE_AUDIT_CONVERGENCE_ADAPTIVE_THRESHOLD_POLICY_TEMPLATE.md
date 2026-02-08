# Archive Audit Convergence Adaptive Threshold Policy Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| policy_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| convergence_report | `<path>` |
| linkage_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| risk_convergence_status | `<pass/warn/fail/unknown>` |
| convergence_index | `<n%>` |
| trend_alerts | `<n>` |
| linkage_status | `<pass/warn/fail/unknown>` |
| gate_alignment_status | `<pass/warn/fail/unknown>` |
| mismatch_rows | `<n>` |
| missing_payload_rows | `<n>` |
| writeback_signaled_items | `<n>` |
| writeback_changed_items | `<n>` |

## 3) Adaptation Summary

| metric | value |
|--------|-------|
| pressure_score | `<n>` |
| adaptation_mode | `<tighten/reinforce/hold/relax>` |
| adaptation_reason | `<reason>` |
| adaptive_status | `<pass/warn/fail>` |
| release_guidance | `<guidance>` |

## 4) Threshold Recommendation

| threshold | base_value | recommended_value | delta |
|-----------|------------|-------------------|-------|
| due_soon_warn_threshold | `<n>` | `<n>` | `<n>` |
| blocking_high_threshold | `<n>` | `<n>` | `<n>` |
| checklist_warn_threshold | `<n>` | `<n>` | `<n>` |
| trend_alert_threshold | `<n>` | `<n>` | `<n>` |

## 5) Decision Queue

| check_id | observed | rule | result |
|----------|----------|------|--------|
| `<check-1>` | `<value>` | `<rule>` | `<pass/review/fail>` |

## 6) Suggested Actions

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
