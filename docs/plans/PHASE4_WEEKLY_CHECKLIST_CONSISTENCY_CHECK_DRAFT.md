# Phase 4 周报与发布清单一致性核查草案（Draft）

**目标**：校验 B29 周报与 B28 发布前清单在 readiness、阻断原因与关键计数上的一致性。  
**阶段**：Batch B33

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_TEMPLATE.md`
- 生成脚本：`scripts/check_archive_audit_weekly_checklist_consistency_draft.sh`

---

## 2. 核查口径

- 周报侧：`weekly_status`、`checklist_readiness_fail`、`checklist_readiness_warn`、`hold_overdue_total`、`linkage_risk_total`。
- 清单侧：`readiness`、`blocking_reasons`、`hold_overdue`。
- 一致性检查：
  - checklist row 映射存在性
  - readiness 字段回显一致
  - blocking reasons 集合一致
  - weekly 状态与 fail/warn 计数规则一致
- 输出：`consistency_status`（pass/warn/fail）+ `release_recommendation`。

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/check_archive_audit_weekly_checklist_consistency_draft.sh \
  --dry-run \
  --consistency-id b33_dryrun_sample

# 生成样例一致性报告
bash scripts/check_archive_audit_weekly_checklist_consistency_draft.sh \
  --weekly-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md \
  --checklist-report docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md \
  --consistency-id b33_sample_20260207_0900 \
  --output docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_SAMPLE_B33.md

# 严格模式（一致性非 pass 则失败）
bash scripts/check_archive_audit_weekly_checklist_consistency_draft.sh \
  --weekly-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md \
  --checklist-report /tmp/checklist_inconsistent.md \
  --strict
```

---

## 4. 验收口径（B33）

- 支持 `--weekly-report/--checklist-report/--strict/--dry-run`。
- 输出包含一致性检查明细（check_id/expected/actual）。
- strict 模式可作为“周报与发布清单一致性门禁”草案。

---

## 5. 后续任务

- B34：状态看板阈值与升级策略草案。
- B35：风险响应执行回执模板草案。
- B36：阻断项关闭校验与豁免记录草案。
