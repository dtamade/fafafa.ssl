# Phase 4 门禁与归档证据模板统一化草案（Draft）

**目标**：统一 Linux/macOS/Windows 的门禁证据表达结构，减少跨平台评审语义差异。  
**阶段**：Batch B16

---

## 1. 统一化范围

本批次统一以下两类产物：

1. **静态模板**：`docs/test_reports/GATE_ARCHIVE_EVIDENCE_TEMPLATE.md`
2. **生成脚本**：`scripts/generate_gate_archive_evidence_template_draft.sh`

统一字段覆盖：

- metadata（平台、profile、run_id、focus_layer）
- gate layer 结果（L0~L3）
- command evidence（命令、退出码、日志路径）
- archive mapping evidence（class 与 retention）
- 决策字段（merge/release 是否阻断）

---

## 2. 为什么要统一

- 让 PR / Nightly / Release 三类门禁报告可直接横向对比。
- 让 B11 归档清单（manifest）与门禁判定形成闭环。
- 让自动化后续（B18/B19）可基于固定字段做质量检查。

---

## 3. 使用方式（Draft）

```bash
# 先看生成计划
bash scripts/generate_gate_archive_evidence_template_draft.sh --dry-run

# 生成 Linux PR 报告骨架
bash scripts/generate_gate_archive_evidence_template_draft.sh \
  --platform linux \
  --profile pr \
  --gate-layer L1 \
  --run-id sample_linux_pr

# 生成 Windows Release 报告骨架
bash scripts/generate_gate_archive_evidence_template_draft.sh \
  --platform windows \
  --profile release \
  --gate-layer L3 \
  --run-id sample_windows_release
```

---

## 4. 验收口径（B16）

- 模板文件包含 `metadata + L0~L3 + archive mapping + decision` 核心区段。
- 生成脚本支持 `platform/profile/gate-layer/run-id` 参数化。
- dry-run 返回 0，且可生成示例报告文件。

---

## 5. 与后续任务关系

- B17：基于模板字段，定义发布级保留策略与清理窗口。
- B18：基于模板与 manifest，补齐归档清理自动化命令。
- B19：基于统一字段产出合规核查清单。
