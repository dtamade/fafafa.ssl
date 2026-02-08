# Phase 4 签批后自动回写执行回执草案（Draft）

**目标**：依据 B39 签批链路与 B40 重测门禁，自动生成执行回执回写载荷并输出放行建议。  
**阶段**：Batch B42

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_TEMPLATE.md`
- 生成脚本：`scripts/writeback_archive_audit_execution_receipt_after_approval_draft.sh`

---

## 2. 回写口径

- 输入来源：
  - 执行回执：`Execution Receipt Rows`
  - 签批链路：`approval_status`、`release_decision`
  - 重测门禁：`Retest Rows`、`regression_gate_status`
- 回写映射：
  - `retest=pass` → `writeback_status=done`
  - `retest=waived` → `writeback_status=waived`
  - `retest=warn` → `writeback_status=in-progress`
  - `retest=fail` → `writeback_status=pending`
- 输出字段：
  - `writeback_changed_items`
  - `writeback_close_percent`
  - `writeback_status`
  - `Receipt Writeback Payload`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/writeback_archive_audit_execution_receipt_after_approval_draft.sh \
  --dry-run \
  --writeback-id b42_dryrun_sample

# 生成样例回写报告
bash scripts/writeback_archive_audit_execution_receipt_after_approval_draft.sh \
  --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md \
  --writeback-id b42_sample_20260207_1330 \
  --output docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md

# 严格模式（回写状态非 pass 则失败）
bash scripts/writeback_archive_audit_execution_receipt_after_approval_draft.sh \
  --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md \
  --strict
```

---

## 4. 验收口径（B42）

- 支持 `--execution-receipt/--approval-chain/--retest-gate/--strict/--dry-run`。
- 输出回写明细、回写载荷与未闭环清单。
- strict 模式可作为“签批后执行回执同步门禁”草案。

---

## 5. 后续任务

- B43：签批证据归档一致性巡检草案。
- B44：重测-签批联动回写一致性草案。
- B45：收敛看板阈值自适应策略草案。
