# Phase 4 签批链路 SLA 违约预警草案（Draft）

**目标**：联动 B39 签批链路与 B47 异常处置队列，输出 SLA 违约风险预警与责任人热点。  
**阶段**：Batch B48

---

## 1. 交付物

- 模板：`docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_TEMPLATE.md`
- 生成脚本：`scripts/monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh`

---

## 2. 预警口径

- 输入来源：
  - 签批链路：`approval_status`、`rejected_stages`、`target_sla`
  - 异常处置：`Response Queue`、`critical_high_open`
- 预警分级：
  - `breach-risk-high`：关键链路 fail 且 `<1h`，或 critical 异常未闭环
  - `breach-risk-medium`：high 异常未闭环或阶段 fail
  - `watch`：queued/open 的中风险项
- 输出字段：
  - `critical/high/medium alert` 计数
  - `owner_hotspots`
  - `Alert Rows`
  - `sla_breach_status` 与 `release_advice`

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh \
  --dry-run \
  --alert-id b48_dryrun_sample

# 生成样例 SLA 违约预警报告
bash scripts/monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh \
  --approval-chain-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md \
  --alert-id b48_sample_20260207_1630 \
  --output docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md

# 严格模式（sla_breach_status 非 pass 则失败）
bash scripts/monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh \
  --approval-chain-report docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md \
  --anomaly-response docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md \
  --strict
```

---

## 4. 验收口径（B48）

- 支持 `--approval-chain-report/--anomaly-response/--strict/--dry-run`。
- 输出预警明细与责任人热点清单。
- strict 模式可作为“签批链路 SLA 门禁”草案。

---

## 5. 后续任务

- B49：回写变更覆盖率修复追踪草案。
- B50：联动与回滚演练计划草案。
- B51：异常处置验证演练清单草案。
