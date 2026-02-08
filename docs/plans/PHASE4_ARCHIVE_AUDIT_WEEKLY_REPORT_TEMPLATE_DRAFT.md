# Phase 4 归档审计执行周报模板草案（Draft）

**目标**：聚合 hold 提醒、抽样联动、发布前核查清单，形成可追踪的周级审计摘要。  
**阶段**：Batch B29

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_weekly_report_draft.sh`

---

## 2. 周报口径

- Hold 维度：`overdue` / `due_soon` / `missing_expiry` / `invalid_expiry`。
- Linkage 维度：`sampled_runs_risk` 与 `status`。
- Checklist 维度：`readiness` 与 `blocking_reasons`。
- 输出：周级状态 `weekly_status`（pass/warn/fail）与阻断建议。

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_weekly_report_draft.sh \
  --dry-run \
  --week-id b29_dryrun_sample

# 生成样例周报
bash scripts/generate_archive_audit_weekly_report_draft.sh \
  --hold-report-glob "docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md" \
  --linkage-report-glob "docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md" \
  --checklist-report-glob "docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md" \
  --week-id b29_sample_20260207_0700 \
  --output docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md

# 严格模式（非 pass 则失败）
bash scripts/generate_archive_audit_weekly_report_draft.sh \
  --hold-report-glob "docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md" \
  --linkage-report-glob "docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md" \
  --checklist-report-glob "docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md" \
  --strict
```

---

## 4. 验收口径（B29）

- 支持 `--hold-report-glob/--linkage-report-glob/--checklist-report-glob/--strict/--dry-run`。
- 周报模板可覆盖周级风险聚合与来源追溯。
- strict 模式可用于周报发布门禁草案。

---

## 5. 后续任务

- B30：归档审计状态看板自动汇总草案。
- B31：归档审计风险分级与响应模板草案。
- B32：发布前审计阻断项自动提取草案。
