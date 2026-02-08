# Phase 4 归档审计抽样与 hold 到期提醒联动草案（Draft）

**目标**：将 B23 抽样记录与 B25 到期提醒建立可追踪联动，快速识别“抽样命中且 hold 风险存在”的条目。  
**阶段**：Batch B27

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_hold_linkage_draft.sh`

---

## 2. 联动口径

- 数据源 A：B23 抽样记录的 `Sampled Runs`。
- 数据源 B：B25 提醒报告的 `Hold Review Rows`。
- 关联键：`run_id`。
- 风险状态：`overdue` / `missing-expiry` / `invalid-expiry` / `not-found(when sample_hold=yes)`。

---

## 3. 常用命令

```bash
# 先 dry-run 看参数解析
bash scripts/generate_archive_audit_hold_linkage_draft.sh \
  --dry-run \
  --sampling docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_SAMPLE_B23.md \
  --hold-review docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md \
  --linkage-id b27_dryrun_sample

# 生成一份联动样例
bash scripts/generate_archive_audit_hold_linkage_draft.sh \
  --sampling docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_SAMPLE_B23.md \
  --hold-review docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md \
  --linkage-id b27_sample_20260207_0612 \
  --output docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md
```

---

## 4. 验收口径（B27）

- 模板覆盖 metadata/summary/linkage rows/checklist。
- 脚本支持 `--sampling/--hold-review/--output/--strict/--dry-run`。
- 可在 strict 模式下对风险状态进行门禁判定。

---

## 5. 后续任务

- B28：发布前归档审计最小核查清单自动生成草案。
- B29：归档审计执行周报模板草案。
- B30：归档审计状态看板自动汇总草案。
