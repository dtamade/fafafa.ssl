# Phase 4 回写覆盖率修复闭环验收门禁草案（Draft）

**目标**：把 B49（覆盖率修复追踪）与 B52（SLA/回滚联动）收敛为单一闭环验收门禁，作为发布前可执行 gate。  
**阶段**：Batch B53

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_TEMPLATE.md`
- 生成脚本：`scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh`

---

## 2. 门禁口径

- 输入来源：
  - B49：`tracker_status`、`writeback_change_coverage_percent`、`*gap_items`
  - B52：`linkage_status`、`missing_alert_mappings`、`alert_without_rollback`
  - B46：`versioning_status`、`rollback_candidates`
- 核心阈值：
  - 覆盖率 `>= min_coverage`（默认 100%）
  - `total_gap_items=0`
  - `critical_gap_items=0`
  - `high_gap_items<=max_high_gap`（默认 0）
  - `rollback_candidates=0`
  - `missing_alert_mappings=0`
  - `alert_without_rollback=0`
- 输出视图：
  - `Acceptance Summary`
  - `Gate Checks`
  - `Outstanding Blockers`
  - `Owner Workload`
  - `acceptance_status` 与 `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh \
  --dry-run \
  --gate-id b53_dryrun_sample

# 生成样例门禁报告
bash scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh \
  --tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md \
  --sla-rollback-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --gate-id b53_sample_20260207_2000 \
  --output docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md

# 严格模式（acceptance_status 非 pass 则失败）
bash scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh \
  --tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md \
  --sla-rollback-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --strict
```

---

## 4. 验收口径（B53）

- 支持 `--min-coverage/--max-high-gap/--strict/--dry-run`。
- 输出可追踪的闭环门禁检查表，且具备 blocker 视图。
- strict 模式可直接作为发布前阻断门禁。

---

## 5. 后续任务

- B54：回写覆盖率自动修复脚本草案。
- B55：SLA/回滚联动报告归档验真脚本草案。
- B56：闭环验收失败自动重试分流脚本草案。
