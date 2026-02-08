# Phase 4 证据巡检异常分级处置草案（Draft）

**目标**：聚合 B43 巡检偏差、B45 策略决策与 B46 回滚队列，输出统一异常分级处置清单。  
**阶段**：Batch B47

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_TEMPLATE.md`
- 生成脚本：`scripts/triage_archive_audit_evidence_anomaly_grading_response_draft.sh`

---

## 2. 分级处置口径

- 输入来源：
  - 一致性巡检：`Mismatch Queue`、`audit_status`
  - 阈值策略：`Decision Queue`、`adaptive_status`
  - 回滚计划：`Rollback Queue`、`versioning_status`
- 分级规则：
  - `critical`：写回覆盖缺失/关键链路阻断
  - `high`：高风险偏差与待回滚项
  - `medium`：策略 review 与趋势类告警
  - `low`：观察项
- 输出字段：
  - `critical/high/medium/low count`
  - `Anomaly Rows`
  - `Response Queue`
  - `response_status` 与 `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/triage_archive_audit_evidence_anomaly_grading_response_draft.sh \
  --dry-run \
  --response-id b47_dryrun_sample

# 生成样例异常分级处置报告
bash scripts/triage_archive_audit_evidence_anomaly_grading_response_draft.sh \
  --audit-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_SAMPLE_B43.md \
  --adaptive-policy docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --response-id b47_sample_20260207_1600 \
  --output docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md

# 严格模式（response_status 非 pass 则失败）
bash scripts/triage_archive_audit_evidence_anomaly_grading_response_draft.sh \
  --audit-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_SAMPLE_B43.md \
  --adaptive-policy docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --strict
```

---

## 4. 验收口径（B47）

- 支持 `--audit-report/--adaptive-policy/--versioning-report/--strict/--dry-run`。
- 输出统一异常分级表与处置队列。
- strict 模式可作为“证据异常处置门禁”草案。

---

## 5. 后续任务

- B48：签批链路 SLA 违约预警草案。
- B49：回写变更覆盖率修复追踪草案。
- B50：联动与回滚演练计划草案。
