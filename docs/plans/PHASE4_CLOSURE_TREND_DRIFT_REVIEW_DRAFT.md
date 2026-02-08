# Phase 4 预备：闭环门禁周趋势与漂移复核草案

> **Batch**: B57
> **Status**: complete
> **Created**: 2026-02-07
> **Dependencies**: B53 (闭环门禁)

## 目标

分析闭环门禁报告的周趋势，检测通过率漂移，生成复核报告以支持发布决策。

## 脚本

- **路径**: `scripts/review_closure_gate_weekly_trend_drift_draft.sh`
- **模式**: 默认输出到 stdout，`--output` 指定文件，`--strict` 严格模式

## 命令示例

### 基本用法

```bash
bash scripts/review_closure_gate_weekly_trend_drift_draft.sh \
  --gate-report-glob "docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_*.md" \
  --review-id b57_sample_20260207_2100
```

### 输出到文件

```bash
bash scripts/review_closure_gate_weekly_trend_drift_draft.sh \
  --gate-report-glob "docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_*.md" \
  --review-id b57_sample_20260207_2100 \
  --output docs/test_reports/ARCHIVE_AUDIT_CLOSURE_TREND_SAMPLE_B57.md
```

### 严格模式

```bash
bash scripts/review_closure_gate_weekly_trend_drift_draft.sh \
  --gate-report-glob "..." \
  --review-id ... \
  --strict
```

## 参数说明

| 参数 | 说明 | 默认值 |
|------|------|--------|
| `--gate-report-glob` | 闭环门禁报告 glob 模式 | 必需 |
| `--review-id` | 复核批次 ID | 必需 |
| `--output` | 输出报告路径 | stdout |
| `--weeks` | 分析周数 | 4 |
| `--drift-threshold` | 漂移阈值百分比 | 10 |
| `--strict` | 严格模式：检测到退化漂移则 exit 1 | false |

## 趋势分析逻辑

| 趋势方向 | 判定条件 |
|----------|----------|
| improving | 最后通过率 > 首次通过率 |
| degrading | 最后通过率 < 首次通过率 |
| stable | 通过率无变化 |

## 漂移检测逻辑

| 条件 | 漂移状态 |
|------|----------|
| 漂移百分比 >= 阈值 | detected |
| 漂移百分比 < 阈值 | none |

## 复核状态判定

| 条件 | 复核状态 |
|------|----------|
| 无漂移或趋势改善 | pass |
| 有漂移但非退化 | warn |
| 有漂移且退化 | fail |

## 输出字段

| 字段 | 说明 |
|------|------|
| `reports_analyzed` | 分析的报告数量 |
| `trend_direction` | 趋势方向（improving/degrading/stable） |
| `drift_percent` | 漂移百分比 |
| `first_pass_rate` | 首次通过率 |
| `last_pass_rate` | 最后通过率 |
| `drift_status` | 漂移状态（detected/none） |
| `review_status` | 复核状态（pass/warn/fail） |

## 验收标准

1. 可正确解析多个闭环门禁报告
2. 可计算通过率趋势和漂移
3. `--strict` 模式在退化漂移时返回 exit 1
4. 输出报告格式与模板一致

## 后续扩展

- B58: 自动修复执行后闭环门禁重验脚本
- B59: 归档审计全链路闭环验收报告
- B60: Phase 4 归档审计工具链汇总文档
