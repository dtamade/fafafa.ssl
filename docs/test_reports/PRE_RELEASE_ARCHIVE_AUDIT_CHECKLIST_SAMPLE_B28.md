# Pre-Release Archive Audit Checklist（Draft）

## 1) Metadata

| field | value |
|------|-------|
| checklist_id | b28_sample_20260207_0630 |
| generated_at | 2026-02-07 05:31:33 +0800 |
| gate_summary | docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md |
| hold_review | docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md |
| linkage_report | docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md |
| operator | codex |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| gate_unknown_or_missing_rows | 4 |
| hold_overdue | 1 |
| hold_due_soon | 1 |
| hold_missing_or_invalid_expiry | 1 |
| linkage_sampled_runs_risk | 0 |
| linkage_status | pass |

## 3) Minimal Checklist

| check_item | result | evidence |
|------------|--------|----------|
| Gate 摘要不存在 unknown/missing 风险行 | fail | gate_unknown_or_missing_rows=4 |
| Hold 记录不存在 overdue | fail | hold_overdue=1 |
| Hold 元数据不存在 missing/invalid expiry | fail | hold_missing_or_invalid_expiry=1 |
| 抽样联动风险为 0 且 linkage_status=pass | pass | linkage_sampled_runs_risk=0; linkage_status=pass |
| 关键输入报告文件均可访问 | pass | gate/hold/linkage files present |

## 4) Release Readiness

| field | value |
|------|-------|
| readiness | fail |
| blocking_reasons | gate_has_unknown_or_missing,hold_overdue_exists,hold_expiry_metadata_incomplete |

## 5) Actions

- blocking:
  - <blocking_action_1>
- followup:
  - <followup_action_1>
