# Phase 4 状态看板阈值与升级策略草案（Draft）

**目标**：将 B30 状态看板指标映射到可执行阈值策略，统一输出升级等级、SLA 与处置动作。  
**阶段**：Batch B34

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_TEMPLATE.md`
- 生成脚本：`scripts/evaluate_archive_audit_dashboard_thresholds_draft.sh`

---

## 2. 阈值口径

- 关键输入指标：
  - `hold_overdue_total`、`hold_missing_or_invalid_expiry_total`
  - `checklist_readiness_fail`、`weekly_fail_count`
  - `blocking_reason_total`、`hold_due_soon_total`
- 阈值参数：
  - `due_soon_warn_threshold`（默认 1）
  - `blocking_high_threshold`（默认 3）
  - `checklist_warn_threshold`（默认 1）
- 输出字段：
  - `escalation_level`（low/medium/high/critical）
  - `decision_status`（pass/warn/fail）
  - `release_policy`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/evaluate_archive_audit_dashboard_thresholds_draft.sh \
  --dry-run \
  --policy-id b34_dryrun_sample

# 生成样例阈值策略报告
bash scripts/evaluate_archive_audit_dashboard_thresholds_draft.sh \
  --dashboard docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md \
  --policy-id b34_sample_20260207_0930 \
  --output docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_SAMPLE_B34.md

# 严格模式（升级等级非 low 则失败）
bash scripts/evaluate_archive_audit_dashboard_thresholds_draft.sh \
  --dashboard docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md \
  --strict
```

---

## 4. 验收口径（B34）

- 支持 `--dashboard/--due-soon-warn-threshold/--blocking-high-threshold/--checklist-warn-threshold/--strict/--dry-run`。
- 输出指标级阈值评估结果与升级摘要。
- strict 模式可作为“仅 low escalation 才放行”的门禁草案。

---

## 5. 后续任务

- B35：风险响应执行回执模板草案。
- B36：阻断项关闭校验与豁免记录草案。
- B37：一致性偏差修复建议草案。
