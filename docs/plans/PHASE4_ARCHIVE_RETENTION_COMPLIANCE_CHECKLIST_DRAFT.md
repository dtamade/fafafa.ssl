# Phase 4 归档保留策略合规核查清单草案（Draft）

**目标**：提供一个可重复执行的核查清单，用于验证归档保留策略是否被正确执行。  
**阶段**：Batch B19

---

## 1. 使用场景

- 每周清理窗口前（pre-cleanup）
- 每次 release 归档后（post-release）
- 审计抽样时（audit sampling）

---

## 2. 基础信息核查

- [ ] 核查范围（时间段 / run_id）已明确
- [ ] 对应 profile（pr/nightly/release）已确认
- [ ] 归档根目录存在：`artifacts/ci/`
- [ ] 每个抽样 run 目录存在 `manifest.csv` 与 `manifest.md`

---

## 3. 保留期合规核查

- [ ] `pr` 归档按 30/14/30/7（core/perf/docs/debug）执行
- [ ] `nightly` 归档按 14/30/30/7 执行
- [ ] `release` 归档按 90/90/90/14 执行
- [ ] 超期归档存在清理计划或已完成清理

---

## 4. 清理执行合规核查

- [ ] 清理动作有 dry-run 证据
- [ ] `--apply` 执行有审批记录（若适用）
- [ ] 清理日志包含候选、跳过、删除汇总
- [ ] 误删回滚路径可用（备份/压缩包/镜像）

---

## 5. 豁免（hold）合规核查

- [ ] `hold` 标记原因可追溯（缺陷/审计/事故）
- [ ] `hold` 设定人和时间可追溯
- [ ] `hold` 到期后已复核（续期或解除）

---

## 6. 门禁证据一致性核查

- [ ] 门禁报告模板字段齐全（metadata/L0-L3/decision）
- [ ] 报告中的 run_id 与归档 run_id 一致
- [ ] 报告引用的日志路径在归档中可找到

---

## 7. 结果记录模板

| item | status | evidence path | owner | due date | notes |
|------|--------|---------------|-------|----------|-------|
| retention_policy | pass/fail | `<path>` | `<name>` | `<date>` | |
| cleanup_execution | pass/fail | `<path>` | `<name>` | `<date>` | |
| hold_exception | pass/fail | `<path>` | `<name>` | `<date>` | |
| gate_consistency | pass/fail | `<path>` | `<name>` | `<date>` | |

---

## 8. 验收口径（B19）

- 清单覆盖保留期、清理动作、hold 豁免、门禁证据一致性。
- 清单结果可落地到 owner 与 due date。
- 可被 B20 聚合摘要直接引用。

---

## 9. 后续任务

- B20：跨平台 Gate 结果聚合摘要模板草案。
- B21：归档豁免 hold 标记流程草案。
- B22：归档清理执行记录模板草案。
