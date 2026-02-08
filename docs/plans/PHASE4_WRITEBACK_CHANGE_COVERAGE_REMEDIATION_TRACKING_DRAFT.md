# Phase 4 回写变更覆盖率修复追踪草案（Draft）

**目标**：聚合 B42/B44/B45/B46/B47/B48 的信号，输出回写变更覆盖率修复追踪队列与责任人负载视图。  
**阶段**：Batch B49

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_TEMPLATE.md`
- 生成脚本：`scripts/track_archive_audit_writeback_change_coverage_remediation_draft.sh`

---

## 2. 修复追踪口径

- 输入来源：
  - 回写执行：`writeback_signaled_items`、`writeback_changed_items`、`Writeback Rows`
  - 联动一致性：`linkage_status`、`mismatch_rows`、`missing_payload_rows`
  - 阈值策略：`Decision Queue`（`writeback-change-coverage`）
  - 回滚与异常：`Rollback Queue`、`Response Queue`
  - SLA 预警：`Alert Rows`（writeback/rollback 相关）
- 追踪分级：
  - `critical`：覆盖率为 0、关键策略 fail、关键 SLA 预警
  - `high`：高风险回滚项与高优先异常
  - `medium`：观察/排队项与中风险信号
- 输出字段：
  - `writeback_change_coverage_percent`
  - `Coverage Gap Rows`
  - `Remediation Queue`
  - `Owner Workload`
  - `tracker_status` 与 `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/track_archive_audit_writeback_change_coverage_remediation_draft.sh \
  --dry-run \
  --tracker-id b49_dryrun_sample

# 生成样例回写覆盖率修复追踪报告
bash scripts/track_archive_audit_writeback_change_coverage_remediation_draft.sh \
  --writeback-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md \
  --adaptive-policy docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md \
  --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \
  --tracker-id b49_sample_20260207_1700 \
  --output docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md

# 严格模式（tracker_status 非 pass 则失败）
bash scripts/track_archive_audit_writeback_change_coverage_remediation_draft.sh \
  --writeback-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md \
  --adaptive-policy docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md \
  --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \
  --strict
```

---

## 4. 验收口径（B49）

- 支持 `--writeback-report/--linkage-report/--adaptive-policy/--versioning-report/--anomaly-response/--sla-alert-report/--strict/--dry-run`。
- 输出覆盖率修复追踪明细、行动队列与责任人负载。
- strict 模式可作为“回写覆盖率修复门禁”草案。

---

## 5. 后续任务

- B50：联动与回滚演练计划草案。
- B51：异常处置验证演练清单草案。
- B52：SLA 与回滚联动演练脚本草案。
