# Phase 4 归档审计状态看板自动汇总草案（Draft）

**目标**：聚合 hold/linkage/checklist/weekly 四类审计报告，自动输出可追踪的状态看板与阻断原因分布。  
**阶段**：Batch B30

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_status_dashboard_draft.sh`

---

## 2. 看板口径

- Hold 维度：`overdue`、`due_soon`、`missing_expiry`、`invalid_expiry`。
- Linkage 维度：`sampled_runs_risk` + `status`。
- Checklist 维度：`readiness` + `blocking_reasons`。
- Weekly 维度：`weekly_status` 与关键失败信号。
- 输出：`dashboard_status`（pass/warn/fail）+ Signal Board + blocking reason 聚合。

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_status_dashboard_draft.sh \
  --dry-run \
  --dashboard-id b30_dryrun_sample

# 生成样例看板
bash scripts/generate_archive_audit_status_dashboard_draft.sh \
  --hold-report-glob "docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md" \
  --linkage-report-glob "docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md" \
  --checklist-report-glob "docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md" \
  --weekly-report-glob "docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md" \
  --dashboard-id b30_sample_20260207_0730 \
  --output docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md

# 严格模式（dashboard_status 非 pass 时失败）
bash scripts/generate_archive_audit_status_dashboard_draft.sh \
  --hold-report-glob "docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md" \
  --linkage-report-glob "docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md" \
  --checklist-report-glob "docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md" \
  --weekly-report-glob "docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md" \
  --strict
```

---

## 4. 验收口径（B30）

- 支持 `--hold-report-glob/--linkage-report-glob/--checklist-report-glob/--weekly-report-glob`。
- 支持 `--strict`（dashboard_status 非 pass 则返回非 0）。
- 输出报告可追溯到各来源文件，并给出阻断原因聚合。

---

## 5. 后续任务

- B31：归档审计风险分级与响应模板草案。
- B32：发布前审计阻断项自动提取草案。
- B33：周报与发布清单一致性核查草案。
