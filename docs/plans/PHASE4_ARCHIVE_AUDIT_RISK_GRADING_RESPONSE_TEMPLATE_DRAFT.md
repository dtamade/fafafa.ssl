# Phase 4 归档审计风险分级与响应模板草案（Draft）

**目标**：基于 B30 状态看板与 B28/B25/B29 报告，统一输出风险分级、响应 SLA 与责任人矩阵。  
**阶段**：Batch B31

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_risk_response_draft.sh`

---

## 2. 分级口径

- 关键输入：
  - Dashboard：`dashboard_status`、`hold_overdue_total`、`checklist_readiness_fail`、`blocking_reason_total`。
  - Checklist：`readiness`、`blocking_reasons`。
  - Hold：`overdue` / `due_soon` / `missing_expiry` / `invalid_expiry`。
  - Weekly：`weekly_status` 与 fail 信号。
- 输出字段：
  - `risk_score`、`overall_risk`（low/medium/high/critical）
  - `decision_status`（pass/warn/fail）
  - `release_advice`
  - Response Matrix（risk_item/severity/owner/sla/response_action）

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_risk_response_draft.sh \
  --dry-run \
  --matrix-id b31_dryrun_sample

# 生成样例风险矩阵
bash scripts/generate_archive_audit_risk_response_draft.sh \
  --dashboard docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md \
  --checklist docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md \
  --hold-review docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md \
  --weekly-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md \
  --matrix-id b31_sample_20260207_0800 \
  --output docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md

# 严格模式（overall_risk 非 low 时失败）
bash scripts/generate_archive_audit_risk_response_draft.sh \
  --dashboard docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md \
  --checklist docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md \
  --hold-review docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md \
  --weekly-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md \
  --strict
```

---

## 4. 验收口径（B31）

- 支持 `--dashboard/--checklist/--hold-review/--weekly-report/--strict/--dry-run`。
- 输出风险分级、响应矩阵与阻断原因列表。
- strict 模式可作为“仅 low 风险才放行”的门禁草案。

---

## 5. 后续任务

- B32：发布前审计阻断项自动提取草案。
- B33：周报与发布清单一致性核查草案。
- B34：状态看板阈值与升级策略草案。
