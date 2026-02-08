# Phase 4 执行回执签批链路草案（Draft）

**目标**：串联 B35/B36/B37/B38 的执行证据，形成可追踪的签批链路与放行决策依据。  
**阶段**：Batch B39

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_execution_approval_chain_draft.sh`

---

## 2. 签批口径

- 输入来源：
  - 执行回执：`execution_readiness`、`release_decision`
  - 关闭校验：`closure_status`、`release_advice`
  - 修复建议：`remediation_status`、`release_guidance`
  - 阈值回测：`backtest_status`、`release_guidance`
- 签批输出：
  - 分阶段状态（`pass/warn/fail/unknown`）
  - `approved/conditional/rejected/pending_review` 计数
  - `approval_status` 与 `release_decision`
  - 非 pass 阶段进入 `Escalation Queue`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_execution_approval_chain_draft.sh \
  --dry-run \
  --chain-id b39_dryrun_sample

# 生成样例签批链路
bash scripts/generate_archive_audit_execution_approval_chain_draft.sh \
  --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md \
  --closure-record docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md \
  --remediation-plan docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_SAMPLE_B37.md \
  --backtest-report docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md \
  --chain-id b39_sample_20260207_1200 \
  --output docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md

# 严格模式（approval_status 非 pass 则失败）
bash scripts/generate_archive_audit_execution_approval_chain_draft.sh \
  --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md \
  --closure-record docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md \
  --remediation-plan docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_SAMPLE_B37.md \
  --backtest-report docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md \
  --strict
```

---

## 4. 验收口径（B39）

- 支持 `--execution-receipt/--closure-record/--remediation-plan/--backtest-report/--strict/--dry-run`。
- 输出签批链路表、升级队列与放行建议。
- strict 模式可作为“发布签批前置门禁”草案。

---

## 5. 后续任务

- B40：阻断项重测与回归门禁草案。
- B41：多周趋势风险收敛看板草案。
- B42：签批后自动回写执行回执草案。
