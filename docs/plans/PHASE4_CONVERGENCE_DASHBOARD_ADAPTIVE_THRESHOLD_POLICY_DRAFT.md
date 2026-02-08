# Phase 4 收敛看板阈值自适应策略草案（Draft）

**目标**：基于 B41 收敛看板与 B44 联动一致性结果，自动产出下一轮阈值策略建议（tighten/reinforce/hold/relax）。  
**阶段**：Batch B45

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_TEMPLATE.md`
- 生成脚本：`scripts/generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh`

---

## 2. 自适应口径

- 输入来源：
  - 收敛看板：`risk_convergence_status`、`convergence_index`、`trend_alerts`
  - 联动报告：`linkage_status`、`mismatch_rows`、`missing_payload_rows`、`writeback_signaled_items/writeback_changed_items`
- 压力评分（`pressure_score`）：
  - convergence/linkage 非 pass 增压
  - mismatch/missing payload 增压
  - writeback 变更覆盖不足增压
- 策略模式：
  - `tighten`：阈值收紧，默认阻断放行
  - `reinforce`：保持主阈值并加强趋势告警
  - `hold`：维持当前阈值
  - `relax`：在高收敛且链路稳定时放宽阈值
- 输出字段：
  - `adaptation_mode`、`adaptive_status`
  - 阈值推荐表（base/recommended/delta）
  - `release_guidance`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh \
  --dry-run \
  --policy-id b45_dryrun_sample

# 生成样例自适应策略
bash scripts/generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh \
  --convergence-report docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md \
  --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md \
  --policy-id b45_sample_20260207_1500 \
  --output docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md

# 严格模式（adaptive_status 非 pass 则失败）
bash scripts/generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh \
  --convergence-report docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md \
  --linkage-report docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md \
  --strict
```

---

## 4. 验收口径（B45）

- 支持 `--convergence-report/--linkage-report/--strict/--dry-run`。
- 输出阈值推荐与决策队列，覆盖收敛与联动双输入。
- strict 模式可作为“阈值自适应策略门禁”草案。

---

## 5. 后续任务

- B46：回写载荷版本化与回滚草案。
- B47：证据巡检异常分级处置草案。
- B48：签批链路 SLA 违约预警草案。
