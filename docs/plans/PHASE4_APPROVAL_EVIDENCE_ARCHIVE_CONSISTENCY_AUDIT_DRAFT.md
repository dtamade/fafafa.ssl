# Phase 4 签批证据归档一致性巡检草案（Draft）

**目标**：对 B39/B40/B41/B42 证据链进行一致性巡检，识别状态回写与归档口径偏差。  
**阶段**：Batch B43

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_TEMPLATE.md`
- 生成脚本：`scripts/check_archive_audit_approval_evidence_consistency_draft.sh`

---

## 2. 巡检口径

- 输入来源：
  - 签批链路：`approval_status`、`rejected_stages`
  - 重测门禁：`regression_gate_status`、`retest_failed`
  - 回写报告：`writeback_status`、`writeback_pending_items`、`writeback_changed_items`
  - 收敛看板：`risk_convergence_status`、`convergence_index`
- 一致性检查：
  - approval 与 retest 状态兼容性
  - retest 失败与 writeback pending 对齐
  - convergence 对上游 fail 的回显一致性
  - writeback 变更覆盖率
- 输出：
  - `audit_status`（pass/warn/fail）
  - `Mismatch Queue`
  - `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/check_archive_audit_approval_evidence_consistency_draft.sh \
  --dry-run \
  --audit-id b43_dryrun_sample

# 生成样例一致性巡检报告
bash scripts/check_archive_audit_approval_evidence_consistency_draft.sh \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md \
  --writeback docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --convergence-dashboard docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md \
  --audit-id b43_sample_20260207_1400 \
  --output docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_SAMPLE_B43.md

# 严格模式（audit_status 非 pass 则失败）
bash scripts/check_archive_audit_approval_evidence_consistency_draft.sh \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md \
  --writeback docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --convergence-dashboard docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md \
  --strict
```

---

## 4. 验收口径（B43）

- 支持 `--approval-chain/--retest-gate/--writeback/--convergence-dashboard/--strict/--dry-run`。
- 输出检查明细、Mismatch Queue 与放行建议。
- strict 模式可作为“签批证据归档一致性门禁”草案。

---

## 5. 后续任务

- B44：重测-签批联动回写一致性草案。
- B45：收敛看板阈值自适应策略草案。
- B46：回写载荷版本化与回滚草案。
