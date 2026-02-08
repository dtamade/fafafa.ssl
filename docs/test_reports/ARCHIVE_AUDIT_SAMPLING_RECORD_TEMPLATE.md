# Archive Audit Sampling Record Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| sample_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| artifact_root | `<path>` |
| profile_filter | `<all|pr|nightly|release>` |
| sampling_method | `<oldest-first|newest-first|manual>` |
| sample_size | `<n>` |
| population_size | `<n>` |
| selected_count | `<n>` |
| operator | `<name_or_ci_job>` |

## 2) Sampling Command

```bash
<sampling command>
```

## 3) Population Snapshot

| profile | run_count | hold_count |
|---------|-----------|------------|
| `<pr|nightly|release|unknown>` | `<n>` | `<n>` |

## 4) Sampled Runs

| run_id | profile | age_days | hold | manifest | source_path |
|--------|---------|----------|------|----------|-------------|
| `<run_id>` | `<profile>` | `<n>` | `<yes/no>` | `<yes/no>` | `<path>` |

## 5) Audit Checklist

- [ ] 抽样来源与筛选条件可复现。
- [ ] hold 样本已记录原因与到期复核日期。
- [ ] 样本归档符合保留策略（B17/B19）。
- [ ] 样本证据可关联 Gate 汇总与清理执行记录。

## 6) Findings & Actions

- findings:
  - `<observation_1>`
- actions:
  - `<followup_action_1>`

## 7) Attachments

- `<cross_platform_gate_summary_path>`
- `<cleanup_execution_record_path>`
- `<audit_log_path>`
