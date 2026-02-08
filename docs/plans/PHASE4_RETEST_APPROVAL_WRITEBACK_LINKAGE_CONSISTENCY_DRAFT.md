# Phase 4 重测-签批联动回写一致性草案（Draft）

**目标**：校验 B40 重测结果到 B42 回写载荷的映射一致性，并确认与 B39 签批状态联动一致。  
**阶段**：Batch B44

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_TEMPLATE.md`
- 生成脚本：`scripts/validate_archive_audit_retest_approval_writeback_linkage_draft.sh`

---

## 2. 联动口径

- 输入来源：
  - 重测门禁：`Retest Rows`
  - 回写报告：`Receipt Writeback Payload`
  - 签批链路：`approval_status`
- 映射规则：
  - `retest=pass` → `writeback=done`
  - `retest=waived` → `writeback=waived`
  - `retest=warn` → `writeback=in-progress`
  - `retest=fail` → `writeback=pending`
- 输出字段：
  - `matched_rows/mismatch_rows/missing_payload_rows`
  - `gate_alignment_status`
  - `linkage_status`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/validate_archive_audit_retest_approval_writeback_linkage_draft.sh \
  --dry-run \
  --linkage-id b44_dryrun_sample

# 生成样例联动一致性报告
bash scripts/validate_archive_audit_retest_approval_writeback_linkage_draft.sh \
  --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --writeback docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --linkage-id b44_sample_20260207_1430 \
  --output docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md

# 严格模式（联动状态非 pass 则失败）
bash scripts/validate_archive_audit_retest_approval_writeback_linkage_draft.sh \
  --retest-gate docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --writeback docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md \
  --strict
```

---

## 4. 验收口径（B44）

- 支持 `--retest-gate/--approval-chain/--writeback/--strict/--dry-run`。
- 输出逐 blocker 联动校验表与 mismatch 队列。
- strict 模式可作为“联动回写一致性门禁”草案。

---

## 5. 后续任务

- B45：收敛看板阈值自适应策略草案。
- B46：回写载荷版本化与回滚草案。
- B47：证据巡检异常分级处置草案。
