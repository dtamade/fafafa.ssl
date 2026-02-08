# Archive Cleanup Execution Record Template（Draft）

## 1) Metadata

| field | value |
|------|-------|
| record_id | `<yyyyMMdd_HHmmss_or_custom>` |
| generated_at | `<YYYY-MM-DD HH:MM:SS +TZ>` |
| profile | `<pr|nightly|release>` |
| mode | `<dry-run|apply>` |
| artifact_root | `<path>` |
| operator | `<name_or_ci_job>` |

## 2) Command

```bash
<cleanup command>
```

## 3) Result Summary

| metric | value |
|--------|-------|
| candidates | `<n>` |
| skipped_hold | `<n>` |
| deleted | `<n>` |
| status | `<pass/fail>` |

## 4) Candidate Details

| run_id | age_days | hold | action |
|--------|----------|------|--------|
| `<run_id>` | `<n>` | `<yes/no>` | `<keep/delete/skip-hold>` |

## 5) Risk Check

- [ ] 不在冻结窗口
- [ ] hold 豁免已核对
- [ ] 关键发布归档未误删

## 6) Attachments

- `<cleanup_log_path>`
- `<manifest_or_backup_path>`
