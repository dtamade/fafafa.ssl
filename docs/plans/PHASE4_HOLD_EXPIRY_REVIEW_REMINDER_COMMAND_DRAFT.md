# Phase 4 hold 到期复核提醒命令草案（Draft）

**目标**：建立 hold 到期提醒命令，持续跟踪 `.hold.meta` 的复核日期与风险状态。  
**阶段**：Batch B25

---

## 1. 交付物

- 命令脚本：`scripts/remind_hold_expiry_review_draft.sh`
- 报告输出：`docs/test_reports/HOLD_EXPIRY_REVIEW_<date>.md`

---

## 2. 覆盖范围

- 扫描 `artifacts/ci/*/.hold.meta`。
- 解析 `owner/reason/expires_on` 字段并计算 `days_left`。
- 分类输出：`ok` / `due-soon` / `overdue` / `missing-expiry` / `invalid-expiry`。
- 严格模式下，存在 `overdue` 时返回非 0。

---

## 3. 常用命令

```bash
# 默认扫描并生成当日报告
bash scripts/remind_hold_expiry_review_draft.sh

# 指定提前提醒窗口与基准日期
bash scripts/remind_hold_expiry_review_draft.sh \
  --days 14 \
  --today 2026-02-07

# 严格模式（存在 overdue 则失败）
bash scripts/remind_hold_expiry_review_draft.sh \
  --days 7 \
  --strict
```

---

## 4. 验收口径（B25）

- 支持 `--days/--today/--strict/--output` 参数。
- 控制台与 Markdown 报告包含一致的统计与行级明细。
- 对缺失/非法日期给出显式分类，避免静默遗漏。

---

## 5. 后续任务

- B26：归档与证据文档索引去重草案。
- B27：归档审计抽样与 hold 到期提醒联动草案。
- B28：发布前归档审计最小核查清单自动生成草案。
