# Phase 4 SLA 与回滚联动演练脚本草案（Draft）

**目标**：在 B50 联动回滚演练计划基础上，补齐“rollback item ↔ SLA alert”映射闭环，产出可执行升级波次与责任人负载视图。  
**阶段**：Batch B52

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_TEMPLATE.md`
- 生成脚本：`scripts/drill_archive_audit_sla_rollback_linkage_draft.sh`

---

## 2. 联动口径

- 输入来源：
  - B48：`Alert Rows`、`sla_breach_status`
  - B46：`Rollback Queue`、`versioning_status`
  - B50：`Rollback Exercise Queue`（回滚预检查与验证动作）
- 映射规则：
  - `rollback_item=BLK-XXX` 对应 `alert_id=RB-BLK-XXX`
  - 缺失映射进入 `Missing SLA Mappings`
  - 仅有告警无回滚项进入 `Alert Rows Without Rollback Items`
- 输出视图：
  - `Linkage Summary`
  - `Linkage Queue`
  - `Escalation Waves`
  - `Owner Workload`
  - `linkage_status` 与 `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/drill_archive_audit_sla_rollback_linkage_draft.sh \
  --dry-run \
  --exercise-id b52_dryrun_sample

# 生成样例报告
bash scripts/drill_archive_audit_sla_rollback_linkage_draft.sh \
  --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \
  --rollback-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --drill-plan-report docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md \
  --exercise-id b52_sample_20260207_1930 \
  --output docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md

# 严格模式（linkage_status 非 pass 则失败）
bash scripts/drill_archive_audit_sla_rollback_linkage_draft.sh \
  --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \
  --rollback-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --drill-plan-report docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md \
  --strict
```

---

## 4. 验收口径（B52）

- 支持 `--sla-alert-report/--rollback-report/--drill-plan-report/--strict/--dry-run`。
- 明确输出 rollback ↔ SLA 的缺口清单（双向）。
- strict 模式可作为后续闭环验收门禁输入。

---

## 5. 后续任务

- B53：回写覆盖率修复闭环验收门禁草案。
- B54：回写覆盖率自动修复脚本草案。
