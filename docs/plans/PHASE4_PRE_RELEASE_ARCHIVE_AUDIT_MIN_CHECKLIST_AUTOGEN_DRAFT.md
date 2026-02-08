# Phase 4 发布前归档审计最小核查清单自动生成草案（Draft）

**目标**：自动汇总 B20/B25/B27 的关键风险指标，生成发布前最小核查清单并给出 readiness 判定。  
**阶段**：Batch B28

---

## 1. 交付物

- 模板：`docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_TEMPLATE.md`
- 生成脚本：`scripts/generate_pre_release_archive_audit_checklist_draft.sh`

---

## 2. 核查口径

- Gate 侧：`unknown/missing` 风险行数量。
- Hold 侧：`overdue`、`due-soon`、`missing/invalid expiry`。
- 联动侧：`sampled_runs_risk` 与 `linkage_status`。
- 输出：最小检查项 + readiness（pass/warn/fail）+ blocking 原因。

---

## 3. 常用命令

```bash
# dry-run 参数检查
bash scripts/generate_pre_release_archive_audit_checklist_draft.sh \
  --dry-run \
  --checklist-id b28_dryrun_sample

# 生成样例清单
bash scripts/generate_pre_release_archive_audit_checklist_draft.sh \
  --gate-summary docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md \
  --hold-review docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md \
  --linkage-report docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md \
  --checklist-id b28_sample_20260207_0630 \
  --output docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md

# 严格模式（readiness 非 pass 时失败）
bash scripts/generate_pre_release_archive_audit_checklist_draft.sh --strict
```

---

## 4. 验收口径（B28）

- 支持 `--gate-summary/--hold-review/--linkage-report/--strict/--dry-run`。
- 核查清单结果可映射到明确的 blocking 原因。
- strict 模式可用于发布前门禁草案。

---

## 5. 后续任务

- B29：归档审计执行周报模板草案。
- B30：归档审计状态看板自动汇总草案。
- B31：归档审计风险分级与响应模板草案。
