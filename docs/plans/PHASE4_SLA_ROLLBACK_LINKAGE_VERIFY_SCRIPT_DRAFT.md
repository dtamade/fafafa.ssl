# Phase 4 预备：SLA/回滚联动报告归档验真脚本草案

> **Batch**: B55
> **Status**: draft
> **Created**: 2026-02-07
> **Dependencies**: B48 (SLA 违约预警), B50 (回滚演练计划), B52 (SLA/回滚联动演练)

## 目标

验证 SLA 预警与回滚演练报告的归档完整性与一致性，确保发布前所有相关证据已正确归档。

## 脚本

- **路径**: `scripts/verify_archive_audit_sla_rollback_linkage_draft.sh`
- **模式**: 默认执行验真检查，`--strict` 模式在失败时返回 exit 1

## 命令示例

### Dry-run 模式

```bash
bash scripts/verify_archive_audit_sla_rollback_linkage_draft.sh \
  --dry-run \
  --verify-id b55_dryrun_sample
```

### 完整验真

```bash
bash scripts/verify_archive_audit_sla_rollback_linkage_draft.sh \
  --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \
  --rollback-drill-report docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md \
  --linkage-drill-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md \
  --verify-id b55_sample_20260207_2100 \
  --output docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_VERIFY_SAMPLE_B55.md
```

### 严格模式

```bash
bash scripts/verify_archive_audit_sla_rollback_linkage_draft.sh \
  --sla-alert-report ... \
  --strict
```

## 参数说明

| 参数 | 说明 | 默认值 |
|------|------|--------|
| `--sla-alert-report` | B48 SLA 违约预警报告 | 可选 |
| `--rollback-drill-report` | B50 回滚演练计划报告 | 可选 |
| `--linkage-drill-report` | B52 SLA/回滚联动演练报告 | 可选 |
| `--archive-root` | 归档根目录 | artifacts/audit |
| `--verify-id` | 验真批次 ID | 必需 |
| `--output` | 输出报告路径 | stdout |
| `--dry-run` | 仅检查，不修复 | false |
| `--strict` | 严格模式 | false |

## 验真检查项

| 检查项 | 说明 |
|--------|------|
| 文件存在性 | 验证报告文件是否存在 |
| 文件非空 | 验证报告文件是否有内容 |
| 归档目录 | 验证归档根目录是否存在 |
| 跨报告联动 | 验证 SLA 与联动报告的项目数量 |
| 时间戳一致性 | 验证报告时间戳是否完整 |

## 输出字段

| 字段 | 说明 |
|------|------|
| `total_checks` | 总检查项数 |
| `pass_checks` | 通过检查数 |
| `fail_checks` | 失败检查数 |
| `warn_checks` | 警告检查数 |
| `verify_status` | 验真状态（pass/warn/fail） |

## 验收标准

1. 可正确检测报告文件存在性与内容
2. 可检测跨报告一致性
3. `--strict` 模式在有失败项时返回 exit 1
4. 输出报告格式与模板一致

## 后续扩展

- B56: 闭环验收失败自动重试分流脚本
- B57: 闭环门禁周趋势与漂移复核
- B58: 自动修复执行后闭环门禁重验脚本
