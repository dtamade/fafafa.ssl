# Phase 4 联动与回滚演练计划草案（Draft）

**目标**：以 B49 覆盖率修复队列为驱动，联动 B46 回滚计划、B47 异常处置与 B48 SLA 预警，形成可执行演练计划。  
**阶段**：Batch B50

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_TEMPLATE.md`
- 生成脚本：`scripts/drill_archive_audit_linkage_rollback_playbook_draft.sh`

---

## 2. 演练口径

- 输入来源：
  - 覆盖率修复追踪：`Remediation Queue`、`tracker_status`
  - 版本化回滚：`rollback_candidates`、`target_version/rollback_version`
  - 异常处置：`Response Queue`、`critical_high_open`
  - SLA 预警：`Alert Rows`、`critical_alert_items`
- 编排规则：
  - 对同一 `item_id + action` 去重，优先保留更高 `priority` 与更高风险 `status`
  - 自动映射演练阶段：`writeback-remediation` / `rollback-execution` / `anomaly-closure`
  - 按优先级估算演练耗时并汇总责任人负载
- 输出字段：
  - `Drill Summary`
  - `Drill Steps`
  - `Rollback Exercise Queue`
  - `Owner Workload`
  - `drill_status` 与 `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/drill_archive_audit_linkage_rollback_playbook_draft.sh \
  --dry-run \
  --drill-id b50_dryrun_sample

# 生成样例联动与回滚演练计划
bash scripts/drill_archive_audit_linkage_rollback_playbook_draft.sh \
  --tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md \
  --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \
  --drill-id b50_sample_20260207_1730 \
  --output docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md

# 严格模式（drill_status 非 pass 则失败）
bash scripts/drill_archive_audit_linkage_rollback_playbook_draft.sh \
  --tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md \
  --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \
  --strict
```

---

## 4. 验收口径（B50）

- 支持 `--tracker-report/--versioning-report/--anomaly-response/--sla-alert-report/--strict/--dry-run`。
- 输出演练步骤、回滚演练队列与责任人负载视图。
- strict 模式可作为“联动与回滚演练门禁”草案。

---

## 5. 后续任务

- B51：异常处置验证演练清单草案。
- B52：SLA 与回滚联动演练脚本草案。
- B53：回写覆盖率修复闭环验收门禁草案。
