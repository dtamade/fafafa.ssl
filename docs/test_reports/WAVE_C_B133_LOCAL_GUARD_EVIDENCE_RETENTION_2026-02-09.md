# Wave C B133 Local Guard Evidence Retention（2026-02-09）

## Current Wave C Chain

- 当前默认入口：`docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
- 当前归档语境：local guard 当前证据根目录以 `tmp/test-reports/` 为准，审批与收口导航以 2026-03-16 主链页为准。
- 历史定位：本页保留 2026-02-09 的保留策略建议，用于归档对照，不再代表默认执行入口。

## 目标

给出 local-first 守护证据归档与保留窗口建议，确保持续自治下的证据可追溯且不失控增长。

## 证据分层建议

### Tier 1（每日必留，7 天）

- `wave_c_b129_oncall_check_*.md`
- `wave_c_b125_local_guard_bundle_*.md`

用途：值班追踪与快速回溯。

### Tier 2（趋势分析，30 天）

- `wave_c_b126_local_guard_history_*.md`
- `wave_c_b124_local_drift_watch_*.md`

用途：识别漂移与稳定性趋势。

### Tier 3（里程碑留档，长期）

- `docs/test_reports/WAVE_C_B12x_*.md`（结果文档）
- `wave_c_b132_local_first_status_snapshot_*.md`

用途：阶段性复盘与交接材料。

## 建议清理窗口

- 每日：清理超过 7 天的 Tier 1 原始 run 报告（保留最近 20 份）。
- 每周：清理超过 30 天的 Tier 2 报告（保留最近 50 份）。
- 每月：确认 Tier 3 文档在 `docs/test_reports` 已完整索引。

## 最小保留策略（建议）

- 始终保留最近一次：B123/B124/B125/B126/B129/B132。
- 始终保留最近 2 次失败样本（若存在），用于故障复盘。
- 索引更新时间必须同步到 `docs/DOCUMENTATION_INDEX.md`。

## 运维执行建议

1. 每日检查：

```bash
bash scripts/run_wave_c_local_guard_oncall_check.sh --strict --quiet
```

2. 每周汇总：

```bash
bash scripts/summarize_wave_c_local_guard_history.sh --strict
```

3. 每周快照：

```bash
bash scripts/generate_wave_c_local_first_status_snapshot.sh --strict
```

## 结论

- B133 完成：local-first 守护证据已具备可执行的分层保留策略建议。
