# Phase 4 多周趋势风险收敛看板草案（Draft）

**目标**：聚合 B38/B39/B40 多周输出，持续跟踪风险是否收敛并输出放行建议。  
**阶段**：Batch B41

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh`

---

## 2. 收敛口径

- 输入来源：
  - backtest：`critical_runs/high_runs/backtest_status`
  - approval chain：`rejected_stages/conditional_stages/approval_status`
  - retest gate：`retest_failed/open_critical_after_retest/regression_gate_status`
- 趋势信号：
  - 对关键指标计算 first/last 差分与方向。
  - 方向上行且超过阈值触发 `alert`。
  - 方向下行且超过阈值标记 `improving`。
- 输出字段：
  - `convergence_index`
  - `trend_alerts`
  - `risk_convergence_status`
  - `release_guidance`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh \
  --dry-run \
  --dashboard-id b41_dryrun_sample

# 生成样例收敛看板
bash scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh \
  --backtest-glob "docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST*.md" \
  --approval-chain-glob "docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN*.md" \
  --retest-gate-glob "docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE*.md" \
  --dashboard-id b41_sample_20260207_1300 \
  --output docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md

# 严格模式（收敛状态非 pass 则失败）
bash scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh \
  --backtest-glob "docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST*.md" \
  --approval-chain-glob "docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN*.md" \
  --retest-gate-glob "docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE*.md" \
  --strict
```

---

## 4. 验收口径（B41）

- 支持 `--backtest-glob/--approval-chain-glob/--retest-gate-glob/--trend-alert-threshold/--strict/--dry-run`。
- 输出多流趋势快照与风险收敛信号。
- strict 模式可作为“多周收敛门禁”草案。

---

## 5. 后续任务

- B42：签批后自动回写执行回执草案。
- B43：签批证据归档一致性巡检草案。
- B44：重测-签批联动回写一致性草案。
