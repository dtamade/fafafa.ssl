# Phase 4 预备：闭环验收失败自动重试分流脚本草案

> **Batch**: B56
> **Status**: draft
> **Created**: 2026-02-07
> **Dependencies**: B53 (闭环门禁), B54 (自动修复), B55 (验真)

## 目标

根据闭环门禁失败项，自动分流重试或升级处理，减少人工干预。

## 脚本

- **路径**: `scripts/retry_closure_acceptance_failure_draft.sh`
- **模式**: 默认 `--dry-run`（仅生成重试计划），`--apply` 实际执行

## 命令示例

### Dry-run 模式

```bash
bash scripts/retry_closure_acceptance_failure_draft.sh \
  --dry-run \
  --retry-id b56_dryrun_sample
```

### 完整执行

```bash
bash scripts/retry_closure_acceptance_failure_draft.sh \
  --closure-gate-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md \
  --autofix-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md \
  --retry-id b56_sample_20260207_2100 \
  --output docs/test_reports/ARCHIVE_AUDIT_CLOSURE_RETRY_SAMPLE_B56.md
```

### 严格模式

```bash
bash scripts/retry_closure_acceptance_failure_draft.sh \
  --closure-gate-report ... \
  --strict
```

## 参数说明

| 参数 | 说明 | 默认值 |
|------|------|--------|
| `--closure-gate-report` | B53 闭环门禁报告 | 必需 |
| `--autofix-report` | B54 自动修复报告 | 可选 |
| `--verify-report` | B55 验真报告 | 可选 |
| `--retry-id` | 重试批次 ID | 必需 |
| `--output` | 输出报告路径 | stdout |
| `--max-retries` | 最大重试次数 | 3 |
| `--retry-delay` | 重试间隔秒数 | 5 |
| `--escalate-threshold` | 升级阈值 | 2 |
| `--dry-run` | 仅生成重试计划 | 默认 |
| `--apply` | 实际执行重试 | - |
| `--strict` | 严格模式 | false |

## 分流逻辑

| 条件 | 动作 |
|------|------|
| 状态为 pass/closed/waived | skip |
| 重试次数 >= max_retries | escalate |
| 重试次数 >= escalate_threshold | escalate |
| 其他 | retry |

## 输出字段

| 字段 | 说明 |
|------|------|
| `total_items` | 总项目数 |
| `retry_items` | 需重试项目数 |
| `escalate_items` | 需升级项目数 |
| `skip_items` | 跳过项目数 |
| `pending_items` | 待处理项目数 |
| `retry_status` | 重试状态（pass/pending/escalate） |

## 验收标准

1. 可正确解析闭环门禁和自动修复报告
2. 可根据重试次数自动分流
3. `--strict` 模式在有未解决项时返回 exit 1
4. 输出报告格式与模板一致

## 后续扩展

- B57: 闭环门禁周趋势与漂移复核
- B58: 自动修复执行后闭环门禁重验脚本
- B59: 归档审计全链路闭环验收报告
