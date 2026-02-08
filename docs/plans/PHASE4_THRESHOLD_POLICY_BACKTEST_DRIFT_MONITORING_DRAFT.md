# Phase 4 阈值策略回测与漂移监控草案（Draft）

**目标**：对多份 B30 看板输出执行阈值策略回测，并监控关键指标漂移风险。  
**阶段**：Batch B38

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_TEMPLATE.md`
- 生成脚本：`scripts/backtest_archive_audit_threshold_policy_draft.sh`

---

## 2. 回测口径

- 输入来源：`ARCHIVE_AUDIT_STATUS_DASHBOARD*.md`。
- 阈值参数：
  - `due_soon_warn_threshold`
  - `blocking_high_threshold`
  - `checklist_warn_threshold`
- 漂移监控：
  - `hold_due_soon_total`
  - `blocking_reason_total`
  - `checklist_readiness_fail`
- 输出字段：
  - `critical/high/medium/low runs`
  - `drift_alerts`
  - `backtest_status` + `release_guidance`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/backtest_archive_audit_threshold_policy_draft.sh \
  --dry-run \
  --backtest-id b38_dryrun_sample

# 生成样例回测报告
bash scripts/backtest_archive_audit_threshold_policy_draft.sh \
  --dashboard-glob "docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD*.md" \
  --backtest-id b38_sample_20260207_1130 \
  --output docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md

# 严格模式（回测状态非 pass 则失败）
bash scripts/backtest_archive_audit_threshold_policy_draft.sh \
  --dashboard-glob "docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD*.md" \
  --strict
```

---

## 4. 验收口径（B38）

- 支持 `--dashboard-glob/--drift-alert-threshold/--strict/--dry-run`。
- 输出逐报告评估表与漂移告警摘要。
- strict 模式可作为阈值策略变更前回测门禁草案。

---

## 5. 后续任务

- B39：执行回执签批链路草案。
- B40：阻断项重测与回归门禁草案。
- B41：多周趋势风险收敛看板草案。
