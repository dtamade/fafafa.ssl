# Wave C One-Page Runbook（B121）

## Current Wave C Chain

- 当前默认入口：`docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
- 当前默认策略：先停在 local-first / approval 链路，workflow 仍保持 `DISABLED`，不把本页作为默认执行 runbook。
- 历史定位：本页保留 2026-02-08 的 trigger-oriented runbook，用于归档对照。

## 1) 触发前（Preflight）

```bash
bash scripts/check_wave_c_first_run_preflight.sh --strict
```

期望：`state=READY`

## 2) 手动触发 Workflow

GitHub Actions → `Wave C Quick Sprint Manual (Template)` → Run workflow

输入建议：

- `run_validation=true`
- `strict_bundle=true`

## 3) 触发后 15 分钟检查

```bash
bash scripts/check_wave_c_post_trigger_observability.sh --run-id <RUN_ID> --strict
```

期望：`state=READY`

## 4) 灰度与回滚门禁

```bash
bash scripts/evaluate_wave_c_b101_thresholds.sh --strict
bash scripts/check_wave_c_default_on_readiness.sh --strict
bash scripts/run_wave_c_b110_rollback_drill.sh --run-id <RUN_ID> --strict
```

## 5) 应急停用 Workflow

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh disable
```

恢复启用：

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh enable
```

## 6) 策略边界

- 当前策略：`DEFAULT_OFF`
- 若要切换 default-on：必须走单独审批，不在当前自动链路内执行。
