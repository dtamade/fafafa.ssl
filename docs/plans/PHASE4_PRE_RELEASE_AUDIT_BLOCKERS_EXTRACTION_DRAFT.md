# Phase 4 发布前审计阻断项自动提取草案（Draft）

**目标**：从 B28/B29/B30/B31 报告自动提取阻断项，输出可执行的发布阻断清单。  
**阶段**：Batch B32

---

## 1. 交付物

- 模板：`docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_TEMPLATE.md`
- 生成脚本：`scripts/extract_pre_release_audit_blockers_draft.sh`

---

## 2. 提取口径

- Checklist 来源：`readiness`、`blocking_reasons`。
- Weekly 来源：`weekly_status`、`hold_overdue_total`、`checklist_readiness_fail`。
- Risk Matrix 来源：`overall_risk`、`decision_status`、Response Matrix 的 high/critical 项。
- Dashboard 来源：`dashboard_status`、`blocking_reason_total`。
- 输出：
  - `blockers_total`、`blockers_status`、`release_gate_decision`
  - 结构化阻断项清单（source/key/severity/owner/action/evidence）

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/extract_pre_release_audit_blockers_draft.sh \
  --dry-run \
  --blocker-id b32_dryrun_sample

# 生成样例阻断清单
bash scripts/extract_pre_release_audit_blockers_draft.sh \
  --checklist docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md \
  --weekly-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md \
  --risk-matrix docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md \
  --dashboard docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md \
  --blocker-id b32_sample_20260207_0830 \
  --output docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md

# 严格模式（阻断状态非 pass 时失败）
bash scripts/extract_pre_release_audit_blockers_draft.sh \
  --checklist docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md \
  --weekly-report docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md \
  --risk-matrix docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md \
  --dashboard docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md \
  --strict
```

---

## 4. 验收口径（B32）

- 支持 `--checklist/--weekly-report/--risk-matrix/--dashboard/--strict/--dry-run`。
- 可从多源报告自动抽取阻断项，并给出 `release_gate_decision`。
- strict 模式可作为发布前阻断门禁草案。

---

## 5. 后续任务

- B33：周报与发布清单一致性核查草案。
- B34：状态看板阈值与升级策略草案。
- B35：风险响应执行回执模板草案。
