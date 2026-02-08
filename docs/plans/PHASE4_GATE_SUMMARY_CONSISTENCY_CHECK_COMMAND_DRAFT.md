# Phase 4 Gate 聚合摘要一致性检查命令草案（Draft）

**目标**：为跨平台 Gate 聚合摘要提供轻量一致性校验命令，提前暴露统计字段不一致问题。  
**阶段**：Batch B24

---

## 1. 交付物

- 命令脚本：`scripts/check_cross_platform_gate_summary_consistency_draft.sh`
- 校验对象：`docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_*.md`

---

## 2. 校验规则

- `metadata.input_reports` 与输入报告行数一致。
- `Layer Signal Snapshot` 行数应等于 `input_reports * 4`（L0~L3）。
- `Platform Aggregate` 行数应等于唯一平台数。
- 输出 `unknown/missing` 行数作为数据完整性提示。

---

## 3. 常用命令

```bash
# 默认检查 B20 样例
bash scripts/check_cross_platform_gate_summary_consistency_draft.sh

# 检查指定摘要文件
bash scripts/check_cross_platform_gate_summary_consistency_draft.sh \
  --summary docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md

# 严格模式（有不一致即失败）
bash scripts/check_cross_platform_gate_summary_consistency_draft.sh \
  --summary docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md \
  --strict
```

---

## 4. 验收口径（B24）

- 支持 `--summary/--strict` 参数。
- 对关键统计字段给出 pass/warn 结果。
- 严格模式可用于 CI 门禁草案联动。

---

## 5. 后续任务

- B25：hold 到期复核提醒命令草案。
- B26：归档与证据文档索引去重草案。
- B27：归档审计抽样与 hold 到期提醒联动草案。
