# Phase 4 预备：回写覆盖率自动修复脚本草案

> **Batch**: B54
> **Status**: draft
> **Created**: 2026-02-07
> **Dependencies**: B53 (闭环门禁), B49 (覆盖率追踪), B46 (版本化回滚), B52 (SLA/回滚联动)

## 目标

提供自动化脚本，根据 B53 闭环门禁输出的未闭环阻断项，自动生成并执行修复动作。

## 脚本

- **路径**: `scripts/autofix_archive_audit_writeback_coverage_draft.sh`
- **模式**: 默认 `--dry-run`（仅生成修复计划），`--apply` 实际执行

## 命令示例

### Dry-run 模式

```bash
bash scripts/autofix_archive_audit_writeback_coverage_draft.sh \
  --dry-run \
  --autofix-id b54_dryrun_sample
```

### 完整执行

```bash
bash scripts/autofix_archive_audit_writeback_coverage_draft.sh \
  --closure-gate-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md \
  --tracker-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md \
  --versioning-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md \
  --sla-rollback-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md \
  --autofix-id b54_sample_20260207_2000 \
  --output docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md
```

### 严格模式

```bash
bash scripts/autofix_archive_audit_writeback_coverage_draft.sh \
  --closure-gate-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md \
  --autofix-id b54_strict_test \
  --strict
```

## 参数说明

| 参数 | 说明 | 默认值 |
|------|------|--------|
| `--closure-gate-report` | B53 闭环门禁报告 | 必需 |
| `--tracker-report` | B49 覆盖率修复追踪报告 | 可选 |
| `--versioning-report` | B46 版本化回滚报告 | 可选 |
| `--sla-rollback-report` | B52 SLA/回滚联动报告 | 可选 |
| `--autofix-id` | 修复批次 ID | 必需 |
| `--output` | 输出报告路径 | stdout |
| `--max-actions` | 最大修复动作数 | 50 |
| `--owner-filter` | 按责任人过滤 | 全部 |
| `--priority-filter` | 按优先级过滤 | critical,high |
| `--dry-run` | 仅生成修复计划 | 默认 |
| `--apply` | 实际执行修复 | - |
| `--strict` | 严格模式 | false |

## 输出字段

| 字段 | 说明 |
|------|------|
| `total_actions` | 总修复动作数 |
| `executed_actions` | 已执行动作数 |
| `simulated_actions` | 模拟动作数（dry-run） |
| `failed_actions` | 失败动作数 |
| `autofix_status` | 修复状态（pass/pending） |

## 验收标准

1. `--dry-run` 模式可正确解析 B53 报告并生成修复计划
2. `--apply` 模式可执行修复动作（当前为占位实现）
3. `--strict` 模式在有未闭环项时返回 exit 1
4. 输出报告格式与模板一致

## 后续扩展

- B55: SLA/回滚联动报告归档验真脚本
- B56: 闭环验收失败自动重试分流脚本
- B57: 闭环门禁周趋势与漂移复核
