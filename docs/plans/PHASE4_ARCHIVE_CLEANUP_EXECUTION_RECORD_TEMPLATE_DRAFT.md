# Phase 4 归档清理执行记录模板草案（Draft）

**目标**：提供统一的清理执行记录格式，保证清理动作可追溯、可审阅。  
**阶段**：Batch B22

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_CLEANUP_EXECUTION_RECORD_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_cleanup_execution_record_draft.sh`

---

## 2. 模板覆盖字段

- metadata（record_id / profile / mode / operator）
- command（执行命令）
- result summary（candidates/skipped/deleted/status）
- risk check（冻结窗口/hold/发布归档）
- attachments（日志与清单路径）

---

## 3. 常用命令

```bash
# 先 dry-run 看参数解析
bash scripts/generate_archive_cleanup_execution_record_draft.sh \
  --dry-run \
  --profile release \
  --mode dry-run \
  --record-id b22_dryrun_sample

# 生成一份样例执行记录
bash scripts/generate_archive_cleanup_execution_record_draft.sh \
  --profile pr \
  --mode dry-run \
  --record-id b22_sample_20260207_0528 \
  --operator codex \
  --command "bash scripts/cleanup_ci_artifacts_draft.sh --profile pr --older-than-days 0 --dry-run" \
  --candidates 1 \
  --skipped-hold 0 \
  --deleted 0 \
  --output docs/test_reports/ARCHIVE_CLEANUP_EXECUTION_RECORD_SAMPLE_B22.md
```

---

## 4. 验收口径（B22）

- 模板可覆盖清理前后审阅所需字段。
- 生成脚本支持 profile/mode/统计值参数化。
- dry-run 与实际生成均可执行。

---

## 5. 后续任务

- B23：归档审计抽样记录草案。
- B24：Gate 聚合摘要一致性检查命令草案。
- B25：hold 到期复核提醒命令草案。
