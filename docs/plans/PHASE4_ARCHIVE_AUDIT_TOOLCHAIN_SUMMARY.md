# Phase 4 归档审计工具链汇总文档

> **Batch**: B60
> **Status**: complete
> **Created**: 2026-02-07
> **Version**: 1.0

## 概述

本文档汇总 Phase 4 归档审计工具链的所有脚本、模板和报告，提供完整的工具链索引和使用指南。

## 工具链架构

```
┌─────────────────────────────────────────────────────────────────┐
│                    Phase 4 归档审计工具链                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐         │
│  │ B20-B27     │    │ B28-B38     │    │ B39-B50     │         │
│  │ 基础设施    │ -> │ 审计与监控  │ -> │ 签批与联动  │         │
│  └─────────────┘    └─────────────┘    └─────────────┘         │
│         │                  │                  │                 │
│         v                  v                  v                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐         │
│  │ B51-B52     │    │ B53-B56     │    │ B57-B59     │         │
│  │ 演练与验证  │ -> │ 闭环门禁    │ -> │ 趋势与汇总  │         │
│  └─────────────┘    └─────────────┘    └─────────────┘         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 脚本索引

### 基础设施层 (B20-B27)

| Batch | 脚本 | 功能 |
|-------|------|------|
| B16 | `generate_gate_archive_evidence_template_draft.sh` | 门禁证据模板生成 |
| B18 | `cleanup_ci_artifacts_draft.sh` | CI 产物清理 |
| B20 | `generate_cross_platform_gate_summary_draft.sh` | 跨平台门禁聚合 |
| B21 | `mark_ci_artifact_hold_draft.sh` | 归档豁免标记 |
| B22 | `generate_archive_cleanup_execution_record_draft.sh` | 清理执行记录 |
| B23 | `generate_archive_audit_sampling_record_draft.sh` | 审计抽样记录 |
| B24 | `check_cross_platform_gate_summary_consistency_draft.sh` | 聚合一致性检查 |
| B25 | `remind_hold_expiry_review_draft.sh` | hold 到期提醒 |
| B26 | `check_docs_index_dedup_draft.sh` | 文档索引去重 |
| B27 | `generate_archive_audit_hold_linkage_draft.sh` | 抽样与 hold 联动 |

### 审计与监控层 (B28-B38)

| Batch | 脚本 | 功能 |
|-------|------|------|
| B28 | `generate_pre_release_archive_audit_checklist_draft.sh` | 发布前审计清单 |
| B29 | `generate_archive_audit_weekly_report_draft.sh` | 审计周报 |
| B30 | `generate_archive_audit_status_dashboard_draft.sh` | 状态看板 |
| B31 | `generate_archive_audit_risk_response_draft.sh` | 风险响应模板 |
| B32 | `extract_pre_release_audit_blockers_draft.sh` | 阻断项提取 |
| B33 | `check_archive_audit_weekly_checklist_consistency_draft.sh` | 周报一致性检查 |
| B34 | `evaluate_archive_audit_dashboard_thresholds_draft.sh` | 看板阈值评估 |
| B35 | `generate_archive_audit_risk_execution_receipt_draft.sh` | 风险执行回执 |
| B36 | `validate_archive_audit_blocker_closure_waiver_draft.sh` | 阻断项关闭校验 |
| B37 | `generate_archive_audit_consistency_remediation_draft.sh` | 一致性修复建议 |
| B38 | `backtest_archive_audit_threshold_policy_draft.sh` | 阈值策略回测 |

### 签批与联动层 (B39-B50)

| Batch | 脚本 | 功能 |
|-------|------|------|
| B39 | `generate_archive_audit_execution_approval_chain_draft.sh` | 执行签批链路 |
| B40 | `run_archive_audit_blocker_retest_regression_gate_draft.sh` | 阻断项重测门禁 |
| B41 | `generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh` | 多周趋势看板 |
| B42 | `writeback_archive_audit_execution_receipt_after_approval_draft.sh` | 签批后回写 |
| B43 | `check_archive_audit_approval_evidence_consistency_draft.sh` | 签批证据一致性 |
| B44 | `validate_archive_audit_retest_approval_writeback_linkage_draft.sh` | 重测签批联动 |
| B45 | `generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh` | 阈值自适应策略 |
| B46 | `manage_archive_audit_writeback_payload_versioning_rollback_draft.sh` | 回写版本化回滚 |
| B47 | `triage_archive_audit_evidence_anomaly_grading_response_draft.sh` | 异常分级处置 |
| B48 | `monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh` | SLA 违约预警 |
| B49 | `track_archive_audit_writeback_change_coverage_remediation_draft.sh` | 覆盖率修复追踪 |
| B50 | `drill_archive_audit_linkage_rollback_playbook_draft.sh` | 联动回滚演练 |

### 演练与验证层 (B51-B52)

| Batch | 脚本 | 功能 |
|-------|------|------|
| B51 | (主线代码) | OCSP stapling 验证链路硬化 |
| B52 | `drill_archive_audit_sla_rollback_linkage_draft.sh` | SLA 回滚联动演练 |

### 闭环门禁层 (B53-B56)

| Batch | 脚本 | 功能 |
|-------|------|------|
| B53 | `validate_archive_audit_writeback_coverage_closure_gate_draft.sh` | 闭环验收门禁 |
| B54 | `autofix_archive_audit_writeback_coverage_draft.sh` | 自动修复脚本 |
| B55 | `verify_archive_audit_sla_rollback_linkage_draft.sh` | SLA 回滚验真 |
| B56 | `retry_closure_acceptance_failure_draft.sh` | 失败重试分流 |

### 趋势与汇总层 (B57-B59)

| Batch | 脚本 | 功能 |
|-------|------|------|
| B57 | `review_closure_gate_weekly_trend_drift_draft.sh` | 周趋势漂移复核 |
| B58 | `revalidate_closure_gate_after_autofix_draft.sh` | 修复后重验 |
| B59 | `generate_archive_audit_full_chain_closure_report_draft.sh` | 全链路闭环报告 |

## 报告模板索引

### 基础模板

| 模板 | 用途 |
|------|------|
| `GATE_ARCHIVE_EVIDENCE_TEMPLATE.md` | 门禁证据归档 |
| `CROSS_PLATFORM_GATE_SUMMARY_TEMPLATE.md` | 跨平台聚合 |
| `ARCHIVE_CLEANUP_EXECUTION_RECORD_TEMPLATE.md` | 清理执行记录 |
| `ARCHIVE_AUDIT_SAMPLING_RECORD_TEMPLATE.md` | 审计抽样 |
| `ARCHIVE_AUDIT_HOLD_LINKAGE_TEMPLATE.md` | hold 联动 |

### 审计模板

| 模板 | 用途 |
|------|------|
| `PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_TEMPLATE.md` | 发布前清单 |
| `ARCHIVE_AUDIT_WEEKLY_REPORT_TEMPLATE.md` | 审计周报 |
| `ARCHIVE_AUDIT_STATUS_DASHBOARD_TEMPLATE.md` | 状态看板 |
| `ARCHIVE_AUDIT_RISK_RESPONSE_TEMPLATE.md` | 风险响应 |
| `PRE_RELEASE_AUDIT_BLOCKERS_TEMPLATE.md` | 阻断项 |

### 签批模板

| 模板 | 用途 |
|------|------|
| `ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_TEMPLATE.md` | 签批链路 |
| `ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_TEMPLATE.md` | 重测门禁 |
| `ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_TEMPLATE.md` | 多周趋势 |
| `ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_TEMPLATE.md` | 回写回执 |
| `ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_TEMPLATE.md` | 证据一致性 |

### 闭环模板

| 模板 | 用途 |
|------|------|
| `ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_TEMPLATE.md` | 闭环门禁 |
| `ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_TEMPLATE.md` | 自动修复 |
| `ARCHIVE_AUDIT_SLA_ROLLBACK_VERIFY_TEMPLATE.md` | SLA 验真 |
| `ARCHIVE_AUDIT_CLOSURE_RETRY_TEMPLATE.md` | 重试分流 |
| `ARCHIVE_AUDIT_CLOSURE_TREND_TEMPLATE.md` | 趋势复核 |
| `ARCHIVE_AUDIT_CLOSURE_REVALIDATE_TEMPLATE.md` | 修复重验 |
| `ARCHIVE_AUDIT_FULL_CHAIN_CLOSURE_TEMPLATE.md` | 全链路闭环 |

## 使用流程

### 标准发布流程

```
1. 运行闭环门禁 (B53)
   └─> validate_archive_audit_writeback_coverage_closure_gate_draft.sh

2. 如有失败项，运行自动修复 (B54)
   └─> autofix_archive_audit_writeback_coverage_draft.sh

3. 修复后重验 (B58)
   └─> revalidate_closure_gate_after_autofix_draft.sh

4. 检查周趋势 (B57)
   └─> review_closure_gate_weekly_trend_drift_draft.sh

5. 生成全链路报告 (B59)
   └─> generate_archive_audit_full_chain_closure_report_draft.sh
```

### 异常处理流程

```
1. 失败项重试 (B56)
   └─> retry_closure_acceptance_failure_draft.sh

2. SLA 验真 (B55)
   └─> verify_archive_audit_sla_rollback_linkage_draft.sh

3. 回滚演练 (B52)
   └─> drill_archive_audit_sla_rollback_linkage_draft.sh
```

## 统计信息

| 类别 | 数量 |
|------|------|
| 总脚本数 | 40+ |
| 报告模板数 | 30+ |
| 样例报告数 | 40+ |
| 计划文档数 | 40+ |
| 覆盖 Batch | B16-B59 |

## 维护说明

1. 所有脚本均为草案状态（`*_draft.sh`），需根据实际使用反馈进行调整
2. 模板和样例报告位于 `docs/test_reports/` 目录
3. 计划文档位于 `docs/plans/` 目录
4. 脚本支持 `--dry-run` 和 `--strict` 模式

## 后续规划

- B61+: 根据实际使用反馈进行工具链优化
- 考虑将草案脚本升级为正式版本
- 集成到 CI/CD 流水线
