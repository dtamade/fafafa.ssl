# Wave C B131 Local-first Handoff Checklist（2026-02-09）

## Current Wave C Chain

- 当前默认入口：`docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
- 当前默认策略：交接时先确认 local-first 守护链仍健康；若涉及审批状态，再补看 `B148/B149`。
- 历史定位：本页保留 2026-02-09 的最小交接清单，用于归档对照。

## 目标

提供最小交接清单，确保新值班人可以在 5 分钟内接手 local-first 守护链路。

## 交接前必须确认

- [ ] `workflow` 处于禁用状态（`DISABLED`）
- [ ] B125 最近一次报告 `overall PASS`
- [ ] B126 最近一次报告 `trend_state STABLE`
- [ ] B127 故障速查文档可访问
- [ ] B130 值班节奏模板可执行

## 交接命令（按顺序）

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh status
bash scripts/run_wave_c_local_guard_oncall_check.sh --strict --quiet
bash scripts/run_wave_c_local_first_guard_bundle.sh --strict
bash scripts/summarize_wave_c_local_guard_history.sh --strict
```

## 验收口径

- Oncall 单行输出包含：`status=PASS workflow=DISABLED trend=STABLE`
- B125 报告：`overall PASS`
- B126 报告：`trend_state STABLE`

## 失败时升级路径

1. 先执行 B127 故障速查。
2. 若 workflow 状态漂移，立即 `disable` 并复检。
3. 若连续两次失败，升级到维护负责人处理。

## 交接结论模板

```text
handoff_status=READY
workflow=DISABLED
latest_b125=PASS
latest_b126=STABLE
oncall_check=PASS
```

## 结论

- B131 完成：local-first 守护链路已具备最小可执行交接标准。
