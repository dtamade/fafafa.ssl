# Phase 4 风险响应执行回执模板草案（Draft）

**目标**：将 B31 风险矩阵、B32 阻断项清单与 B34 阈值策略合并为统一执行回执，形成风险闭环证据。  
**阶段**：Batch B35

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_risk_execution_receipt_draft.sh`

---

## 2. 回执口径

- 输入来源：
  - Risk Matrix：`overall_risk`、`decision_status`、`release_advice`
  - Blockers：`blocker_code`、`severity`、`owner`、`action`
  - Threshold Policy：`escalation_level`、`release_policy`
- 执行状态：
  - 默认 critical/high 为 `pending`
  - 支持通过 `--close-blockers` 标记 `done`
  - 支持通过 `--waive-blockers` 标记 `waived`
- 输出字段：
  - `execution_readiness`、`release_decision`
  - 执行明细行（含 SLA 与回执备注）
  - 未闭环项清单

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_risk_execution_receipt_draft.sh \
  --dry-run \
  --receipt-id b35_dryrun_sample

# 生成样例执行回执
bash scripts/generate_archive_audit_risk_execution_receipt_draft.sh \
  --risk-matrix docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md \
  --blockers docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md \
  --threshold-policy docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_SAMPLE_B34.md \
  --receipt-id b35_sample_20260207_1000 \
  --output docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md

# 严格模式（执行就绪非 pass 则失败）
bash scripts/generate_archive_audit_risk_execution_receipt_draft.sh \
  --risk-matrix docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md \
  --blockers docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md \
  --threshold-policy docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_SAMPLE_B34.md \
  --strict
```

---

## 4. 验收口径（B35）

- 支持 `--risk-matrix/--blockers/--threshold-policy/--close-blockers/--waive-blockers/--strict/--dry-run`。
- 输出执行回执与未闭环项，支持后续 B36 的关闭校验与豁免记录。
- strict 模式可作为“风险响应执行完成度门禁”草案。

---

## 5. 后续任务

- B36：阻断项关闭校验与豁免记录草案。
- B37：一致性偏差修复建议草案。
- B38：阈值策略回测与漂移监控草案。
