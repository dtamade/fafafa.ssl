# Phase 4 阻断项关闭校验与豁免记录草案（Draft）

**目标**：基于 B35 执行回执，自动校验 blocker 关闭状态并生成豁免记录与放行建议。  
**阶段**：Batch B36

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_TEMPLATE.md`
- 生成脚本：`scripts/validate_archive_audit_blocker_closure_waiver_draft.sh`

---

## 2. 校验口径

- 输入来源：`ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_*`。
- 状态评估：
  - `done` 计为关闭。
  - `waived` 计为豁免关闭并记录 waiver。
  - `pending/in-progress/unknown` 计为未关闭。
- 关键输出：
  - `close_percent`
  - `closure_status`（pass/warn/fail）
  - `critical_unclosed` / `high_unclosed`
  - `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/validate_archive_audit_blocker_closure_waiver_draft.sh \
  --dry-run \
  --record-id b36_dryrun_sample

# 生成样例关闭校验记录
bash scripts/validate_archive_audit_blocker_closure_waiver_draft.sh \
  --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md \
  --record-id b36_sample_20260207_1030 \
  --output docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md

# 严格模式（closure_status 非 pass 则失败）
bash scripts/validate_archive_audit_blocker_closure_waiver_draft.sh \
  --execution-receipt docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md \
  --strict
```

---

## 4. 验收口径（B36）

- 支持 `--execution-receipt/--required-close-percent/--waiver-reason/--strict/--dry-run`。
- 输出关闭校验行、豁免记录、未关闭项明细。
- strict 模式可作为发布前“阻断项关闭完成度门禁”草案。

---

## 5. 后续任务

- B37：一致性偏差修复建议草案。
- B38：阈值策略回测与漂移监控草案。
- B39：执行回执签批链路草案。
