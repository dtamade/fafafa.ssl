# Phase 4 预备：自动修复执行后闭环门禁重验脚本草案

> **Batch**: B58
> **Status**: complete
> **Created**: 2026-02-07
> **Dependencies**: B53 (闭环门禁), B54 (自动修复)

## 目标

在自动修复执行后重新运行闭环门禁验证，确认修复效果，生成重验报告以支持发布决策。

## 脚本

- **路径**: `scripts/revalidate_closure_gate_after_autofix_draft.sh`
- **模式**: 默认执行重验，`--dry-run` 仅生成计划，`--strict` 严格模式

## 命令示例

### Dry-run 模式

```bash
bash scripts/revalidate_closure_gate_after_autofix_draft.sh \
  --dry-run \
  --revalidate-id b58_dryrun_sample
```

### 完整执行

```bash
bash scripts/revalidate_closure_gate_after_autofix_draft.sh \
  --autofix-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md \
  --closure-gate-script scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh \
  --revalidate-id b58_sample_20260207_2200 \
  --output docs/test_reports/ARCHIVE_AUDIT_CLOSURE_REVALIDATE_SAMPLE_B58.md
```

### 严格模式

```bash
bash scripts/revalidate_closure_gate_after_autofix_draft.sh \
  --autofix-report ... \
  --closure-gate-script ... \
  --strict
```

## 参数说明

| 参数 | 说明 | 默认值 |
|------|------|--------|
| `--autofix-report` | B54 自动修复报告 | 必需（非 dry-run） |
| `--closure-gate-script` | 闭环门禁脚本路径 | 必需（非 dry-run） |
| `--closure-gate-args` | 闭环门禁脚本参数 | 可选 |
| `--revalidate-id` | 重验批次 ID | 必需 |
| `--output` | 输出报告路径 | stdout |
| `--dry-run` | 仅生成重验计划 | false |
| `--strict` | 严格模式：重验失败则 exit 1 | false |

## 重验流程

1. 解析自动修复报告，提取修复动作统计
2. 调用闭环门禁脚本执行重验
3. 解析重验结果，计算通过率
4. 生成综合评估报告

## 综合状态判定

| 条件 | 综合状态 |
|------|----------|
| 重验通过 | pass |
| 重验有警告 | warn |
| 重验失败或错误 | fail |
| 未执行重验 | pending |

## 输出字段

| 字段 | 说明 |
|------|------|
| `autofix_status` | 自动修复状态 |
| `total_actions` | 总修复动作数 |
| `applied_actions` | 已应用动作数 |
| `failed_actions` | 失败动作数 |
| `revalidation_status` | 重验状态 |
| `gate_pass_rate` | 门禁通过率 |
| `overall_status` | 综合状态 |

## 验收标准

1. 可正确解析自动修复报告
2. 可调用闭环门禁脚本执行重验
3. `--strict` 模式在重验失败时返回 exit 1
4. 输出报告格式与模板一致

## 后续扩展

- B59: 归档审计全链路闭环验收报告
- B60: Phase 4 归档审计工具链汇总文档
