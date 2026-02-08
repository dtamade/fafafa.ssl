# Archive Audit Sampling Record（Draft）

## 1) Metadata

| field | value |
|------|-------|
| sample_id | b23_sample_20260207_0600 |
| generated_at | 2026-02-07 05:01:53 +0800 |
| artifact_root | /home/dtamade/projects/fafafa.ssl/artifacts/ci |
| profile_filter | all |
| sampling_method | oldest-first |
| sample_size | 1 |
| population_size | 1 |
| selected_count | 1 |
| operator | codex |

## 2) Sampling Command

~~~bash
bash scripts/generate_archive_audit_sampling_record_draft.sh --profile all --method oldest-first --sample-size 1
~~~

## 3) Population Snapshot

| profile | run_count | hold_count |
|---------|-----------|------------|
| unknown | 1 | 0 |

## 4) Sampled Runs

| run_id | profile | age_days | hold | manifest | source_path |
|--------|---------|----------|------|----------|-------------|
| b11_smoke_20260207_0420 | unknown | 0 | no | yes | /home/dtamade/projects/fafafa.ssl/artifacts/ci/b11_smoke_20260207_0420 |

## 5) Audit Checklist

- [ ] 抽样来源与筛选条件可复现。
- [ ] hold 样本已记录原因与到期复核日期。
- [ ] 样本归档符合保留策略（B17/B19）。
- [ ] 样本证据可关联 Gate 汇总与清理执行记录。

## 6) Findings & Actions

- findings:
  - <observation_1>
- actions:
  - <followup_action_1>

## 7) Attachments

- <cross_platform_gate_summary_path>
- <cleanup_execution_record_path>
- <audit_log_path>
