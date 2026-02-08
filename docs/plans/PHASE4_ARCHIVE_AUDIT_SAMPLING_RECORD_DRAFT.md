# Phase 4 归档审计抽样记录草案（Draft）

**目标**：提供统一的归档审计抽样记录格式，支持清理前抽样核查与留痕。  
**阶段**：Batch B23

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_sampling_record_draft.sh`

---

## 2. 模板覆盖字段

- metadata（sample_id / profile_filter / method / sample_size / selected_count）
- sampling command（抽样命令留痕）
- population snapshot（按 profile 的总体规模与 hold 数量）
- sampled runs（run_id / hold / manifest / age）
- audit checklist + findings/actions + attachments

---

## 3. 常用命令

```bash
# 先 dry-run 看参数解析
bash scripts/generate_archive_audit_sampling_record_draft.sh \
  --dry-run \
  --profile release \
  --method oldest-first \
  --sample-size 2 \
  --sample-id b23_dryrun_sample

# 基于实际归档目录生成一份样例
bash scripts/generate_archive_audit_sampling_record_draft.sh \
  --profile all \
  --method oldest-first \
  --sample-size 1 \
  --sample-id b23_sample_20260207_0600 \
  --operator codex \
  --output docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_SAMPLE_B23.md
```

---

## 4. 验收口径（B23）

- 模板覆盖抽样审计最小字段（总体 + 样本 + 检查项）。
- 脚本支持 profile/method/sample-size/manual run-id 参数化。
- dry-run 与样例输出均可执行。

---

## 5. 后续任务

- B24：Gate 聚合摘要一致性检查命令草案。
- B25：hold 到期复核提醒命令草案。
- B26：归档与证据文档索引去重草案。
