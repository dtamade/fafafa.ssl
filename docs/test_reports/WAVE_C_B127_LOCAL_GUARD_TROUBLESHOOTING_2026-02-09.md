# Wave C B127 Local Guard Troubleshooting（2026-02-09）

## 目标

为 local-first 守护链路提供故障速查手册，缩短异常定位与恢复时间。

## 快速诊断顺序

1. 检查 workflow 状态：

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh status
```

期望：`status=DISABLED`

2. 检查连续性门禁（B123）：

```bash
bash scripts/check_wave_c_local_first_continuity.sh --strict
```

3. 检查周期漂移门禁（B124）：

```bash
bash scripts/check_wave_c_local_drift_watch.sh --strict
```

4. 一键守护链路（B125）：

```bash
bash scripts/run_wave_c_local_first_guard_bundle.sh --strict
```

5. 历史趋势汇总（B126）：

```bash
bash scripts/summarize_wave_c_local_guard_history.sh --strict
```

## 常见故障与处理

### 1) workflow 漂移为 ENABLED

- 现象：B123/B124 报告显示 `workflow_mode=ENABLED`
- 处理：

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh disable
bash scripts/check_wave_c_local_first_continuity.sh --strict
bash scripts/check_wave_c_local_drift_watch.sh --strict
```

### 2) B124 报告 `continuity_age_hours` 超阈值

- 现象：`local_drift_state=HOLD`
- 处理：刷新 B123/B124：

```bash
bash scripts/check_wave_c_local_first_continuity.sh --strict
bash scripts/check_wave_c_local_drift_watch.sh --strict
```

### 3) B124 报告 `bundle_age_hours` 超阈值

- 现象：`latest bundle` 过旧导致 HOLD
- 处理：

```bash
bash scripts/run_wave_c_quick_sprint_bundle.sh --strict
bash scripts/check_wave_c_local_drift_watch.sh --strict
```

### 4) B126 趋势退化为 `DEGRADED`

- 现象：历史中出现 FAIL
- 处理：定位最近失败报告：

```bash
ls -1t test-reports/wave_c_b125_local_guard_bundle_*.md | head -3
```

逐项检查对应 log（B123/B124）并修复后重跑 B125 strict。

## 日常执行建议（Local-first）

- 每日：执行 `B125 strict`
- 每周：执行 `B126 strict`
- 任何策略变更后：立即执行 `B123 + B124 strict`

## 结论

- B127 完成：本地守护链路具备“诊断顺序 + 故障对策 + 日常节奏”三层操作闭环。
