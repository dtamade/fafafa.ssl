# Wave C B130 Oncall Rhythm Template（2026-02-09）

## Current Wave C Chain

- 当前默认入口：`docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
- 当前默认策略：oncall / pre-CI 节奏继续有效，但当前推荐把值班日志落到 `tmp/test-reports`。
- 历史定位：本页保留 2026-02-09 的节奏模板，用于归档对照。

## 目标

为 local-first 守护链路提供可直接落地的值班节奏模板（每日/每周/异常）。

## 每日节奏（建议）

### 每日 09:00

```bash
bash scripts/run_wave_c_local_guard_oncall_check.sh --strict --quiet
```

期望：

```text
WAVE_C_LOCAL_GUARD status=PASS ... workflow=DISABLED trend=STABLE
```

### 每日 17:30

```bash
bash scripts/run_wave_c_local_first_guard_bundle.sh --strict
```

用途：在工作日结束前确认 local-first 门禁无漂移。

## 每周节奏（建议）

### 每周一 10:00

```bash
bash scripts/summarize_wave_c_local_guard_history.sh --strict
```

检查项：

- `trend_state=STABLE`
- 最近 7 天无 FAIL 记录

## 异常处置节奏

当出现 `status=FAIL` 或 `trend=DEGRADED`：

1. 立即执行：

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh status
bash scripts/check_wave_c_local_first_continuity.sh --strict
bash scripts/check_wave_c_local_drift_watch.sh --strict
```

2. 若 workflow 漂移启用：

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh disable
```

3. 复检：

```bash
bash scripts/run_wave_c_local_guard_oncall_check.sh --strict
```

## Cron 示例（Linux）

```cron
# Daily quick oncall check
0 9 * * * cd /path/to/fafafa.ssl && mkdir -p tmp/test-reports && bash scripts/run_wave_c_local_guard_oncall_check.sh --strict --quiet >> tmp/test-reports/wave_c_oncall_quick.log 2>&1

# Daily full local guard bundle
30 17 * * * cd /path/to/fafafa.ssl && mkdir -p tmp/test-reports && bash scripts/run_wave_c_local_first_guard_bundle.sh --strict >> tmp/test-reports/wave_c_oncall_bundle.log 2>&1

# Weekly trend summary
0 10 * * 1 cd /path/to/fafafa.ssl && mkdir -p tmp/test-reports && bash scripts/summarize_wave_c_local_guard_history.sh --strict >> tmp/test-reports/wave_c_oncall_weekly.log 2>&1
```

## 结论

- B130 完成：local-first 守护链路已具备值班节奏模板，可直接接入团队日常运维。
