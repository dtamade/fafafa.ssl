# Wave C B122 CI Deferred / Local Mode（2026-02-08）

## Decision

- CI 暂缓执行（按用户指令）。
- Wave C 手动 workflow 已切换为禁用状态。

## Action Taken

```bash
bash scripts/toggle_wave_c_quick_sprint_workflow.sh disable
bash scripts/toggle_wave_c_quick_sprint_workflow.sh status
```

## Result

- status: `DISABLED`
- path: `.github/workflows/wave-c-quick-sprint-manual.yml.disabled`

## Next (Local-first)

- 继续本地脚本链路与文档收口。
- 待你确认后再恢复 CI：
  - `bash scripts/toggle_wave_c_quick_sprint_workflow.sh enable`
