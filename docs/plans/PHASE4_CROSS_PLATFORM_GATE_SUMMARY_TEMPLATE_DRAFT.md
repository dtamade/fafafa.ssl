# Phase 4 跨平台 Gate 结果聚合摘要模板草案（Draft）

**目标**：将 Linux/macOS/Windows 的 Gate 证据报告聚合为单页摘要，便于审阅与发布决策。  
**阶段**：Batch B20

---

## 1. 交付物

1. 聚合脚本：`scripts/generate_cross_platform_gate_summary_draft.sh`
2. 聚合输出：`docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_<runid>.md`

---

## 2. 聚合维度

- 输入报告 metadata：platform / profile / run_id / focus_layer
- L0~L3 状态快照
- 按平台聚合的 report_count 与 profile 分布

---

## 3. 常用命令

```bash
# Dry-run：查看会解析哪些证据文件
bash scripts/generate_cross_platform_gate_summary_draft.sh \
  --dry-run \
  --input "docs/test_reports/GATE_ARCHIVE_EVIDENCE_*.md"

# 基于 B16 样例生成一份摘要
bash scripts/generate_cross_platform_gate_summary_draft.sh \
  --input "docs/test_reports/GATE_ARCHIVE_EVIDENCE_SAMPLE_B16.md" \
  --run-id b20_sample_20260207_0522 \
  --output docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md
```

---

## 4. 验收口径（B20）

- 脚本支持 `--input/--output/--run-id/--dry-run`。
- 输出文档包含：输入清单、L0~L3 状态快照、平台聚合表。
- 对缺失字段给出 `unknown/missing`，不因部分输入不完整而失败。

---

## 5. 后续任务

- B21：归档豁免 hold 标记流程草案。
- B22：归档清理执行记录模板草案。
- B23：归档审计抽样记录草案。
