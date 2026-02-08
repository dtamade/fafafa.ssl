# Archive Audit Consistency Gap Remediation Plan Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| plan_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| consistency_report | `<path>` |
| closure_record | `<path>` |
| blockers_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| consistency_status | `<pass/warn/fail/unknown>` |
| consistency_critical_fail_count | `<n>` |
| consistency_warning_count | `<n>` |
| closure_status | `<pass/warn/fail/unknown>` |
| closure_critical_unclosed | `<n>` |
| closure_high_unclosed | `<n>` |
| closure_close_percent | `<n%>` |
| blockers_status | `<pass/warn/fail/unknown>` |
| blockers_critical | `<n>` |
| blockers_high | `<n>` |
| blockers_medium | `<n>` |

## 3) Remediation Summary

| metric | value |
|--------|-------|
| critical_actions | `<n>` |
| high_actions | `<n>` |
| medium_actions | `<n>` |
| remediation_status | `<pass/warn/fail>` |
| release_guidance | `<guidance>` |

## 4) Recommended Actions

| priority | area | owner | target_window | suggestion | trigger |
|----------|------|-------|---------------|------------|---------|
| `<critical/high/medium>` | `<area>` | `<owner>` | `<window>` | `<suggestion>` | `<trigger>` |

## 5) Suggested Next Step

- immediate:
  - `<immediate_action_1>`
- followup:
  - `<followup_action_1>`
