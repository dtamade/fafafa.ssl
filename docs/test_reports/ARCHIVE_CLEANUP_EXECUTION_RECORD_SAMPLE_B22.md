# Archive Cleanup Execution Record（Draft）

## 1) Metadata

| field | value |
|------|-------|
| record_id | b22_sample_20260207_0528_fix |
| generated_at | 2026-02-07 04:55:09 +0800 |
| profile | pr |
| mode | dry-run |
| artifact_root | /home/dtamade/projects/fafafa.ssl/artifacts/ci |
| operator | codex |

## 2) Command

~~~bash
bash scripts/cleanup_ci_artifacts_draft.sh --profile pr --older-than-days 0 --dry-run
~~~

## 3) Result Summary

| metric | value |
|--------|-------|
| candidates | 1 |
| skipped_hold | 0 |
| deleted | 0 |
| status | pass |

## 4) Candidate Details

| run_id | age_days | hold | action |
|--------|----------|------|--------|
| <run_id> | <n> | <yes/no> | <keep/delete/skip-hold> |

## 5) Risk Check

- [ ] 不在冻结窗口
- [ ] hold 豁免已核对
- [ ] 关键发布归档未误删

## 6) Attachments

- <cleanup_log_path>
- <manifest_or_backup_path>
