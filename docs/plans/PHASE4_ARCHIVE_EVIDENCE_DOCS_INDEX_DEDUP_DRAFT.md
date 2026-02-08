# Phase 4 归档与证据文档索引去重草案（Draft）

**目标**：为 `docs/DOCUMENTATION_INDEX.md` 提供可重复的去重检查命令，降低 archive/test_reports/plans 入口重复率。  
**阶段**：Batch B26

---

## 1. 交付物

- 命令脚本：`scripts/check_docs_index_dedup_draft.sh`
- 报告输出：`docs/test_reports/DOCS_INDEX_DEDUP_REPORT_<id>.md`

---

## 2. 去重检查口径

- 重复 path：同一路径在索引中出现多次。
- 重复 title：同一标题在索引中出现多次。
- 默认范围：`archive-evidence`（`archive/`、`test_reports/`、`plans/PHASE4_`）。
- 可扩展范围：`all`（全索引扫描）。

---

## 3. 常用命令

```bash
# 默认检查（archive + evidence 范围）
bash scripts/check_docs_index_dedup_draft.sh

# 生成一份样例报告
bash scripts/check_docs_index_dedup_draft.sh \
  --scope archive-evidence \
  --output docs/test_reports/DOCS_INDEX_DEDUP_REPORT_SAMPLE_B26.md

# 严格模式（发现重复即失败）
bash scripts/check_docs_index_dedup_draft.sh \
  --scope all \
  --strict
```

---

## 4. 验收口径（B26）

- 支持 `--scope/--output/--strict` 参数。
- 控制台输出与报告输出具备相同统计维度。
- 严格模式可供后续 CI/预发布文档门禁联动。

---

## 5. 后续任务

- B27：归档审计抽样与 hold 到期提醒联动草案。
- B28：发布前归档审计最小核查清单自动生成草案。
- B29：归档审计执行周报模板草案。
