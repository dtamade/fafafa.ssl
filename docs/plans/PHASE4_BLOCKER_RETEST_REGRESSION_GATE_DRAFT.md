# Phase 4 阻断项重测与回归门禁草案（Draft）

**目标**：基于 B36 关闭校验与 B39 签批链路，对未闭环阻断项输出重测结论并形成回归门禁建议。  
**阶段**：Batch B40

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_TEMPLATE.md`
- 生成脚本：`scripts/run_archive_audit_blocker_retest_regression_gate_draft.sh`

---

## 2. 回归门禁口径

- 输入来源：
  - 关闭校验：`closure_status`、`critical_unclosed`、`high_unclosed`、`Unclosed Items`
  - 签批链路：`approval_status`、`release_decision`、`Escalation Queue`
- 重测规则：
  - 支持 `--retest-pass-blockers` 标记重测通过。
  - 支持 `--retest-waive-blockers` 标记豁免。
  - critical/high 默认仍未通过则判定 `fail`。
- 输出字段：
  - `retest_passed/waived/warn/failed`
  - `regression_gate_status`
  - `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/run_archive_audit_blocker_retest_regression_gate_draft.sh \
  --dry-run \
  --gate-id b40_dryrun_sample

# 生成样例回归门禁报告
bash scripts/run_archive_audit_blocker_retest_regression_gate_draft.sh \
  --closure-record docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --gate-id b40_sample_20260207_1230 \
  --output docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md

# 严格模式（门禁状态非 pass 则失败）
bash scripts/run_archive_audit_blocker_retest_regression_gate_draft.sh \
  --closure-record docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md \
  --approval-chain docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --strict
```

---

## 4. 验收口径（B40）

- 支持 `--closure-record/--approval-chain/--retest-pass-blockers/--retest-waive-blockers/--strict/--dry-run`。
- 输出重测结果表、升级快照与回归门禁建议。
- strict 模式可作为“阻断项重测完成度门禁”草案。

---

## 5. 后续任务

- B41：多周趋势风险收敛看板草案。
- B42：签批后自动回写执行回执草案。
- B43：签批证据归档一致性巡检草案。
