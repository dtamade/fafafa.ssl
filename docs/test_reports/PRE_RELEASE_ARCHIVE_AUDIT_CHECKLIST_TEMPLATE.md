# Pre-Release Archive Audit Checklist Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| checklist_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| gate_summary | `<path>` |
| hold_review | `<path>` |
| linkage_report | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| gate_unknown_or_missing_rows | `<n>` |
| hold_overdue | `<n>` |
| hold_due_soon | `<n>` |
| hold_missing_or_invalid_expiry | `<n>` |
| linkage_sampled_runs_risk | `<n>` |
| linkage_status | `<pass/warn/fail>` |

## 3) Minimal Checklist

| check_item | result | evidence |
|------------|--------|----------|
| Gate 摘要不存在 unknown/missing 风险行 | `<pass/fail>` | `<metric_or_file>` |
| Hold 记录不存在 overdue | `<pass/fail>` | `<metric_or_file>` |
| Hold 元数据不存在 missing/invalid expiry | `<pass/fail>` | `<metric_or_file>` |
| 抽样联动风险为 0 且 linkage_status=pass | `<pass/fail>` | `<metric_or_file>` |
| 关键输入报告文件均可访问 | `<pass/fail>` | `<paths>` |

## 4) Release Readiness

| field | value |
|------|-------|
| readiness | `<pass/warn/fail>` |
| blocking_reasons | `<none_or_reason_list>` |

## 5) Actions

- blocking:
  - `<blocking_action_1>`
- followup:
  - `<followup_action_1>`
