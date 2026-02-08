# Wave C B119 First-Run Checklist（2026-02-08）

## 目标

在首次手动触发 `wave-c-quick-sprint-manual` 前做本地预检，降低触发失败率。

## 快速步骤

1. 运行 preflight：
   - `bash scripts/check_wave_c_first_run_preflight.sh --strict`
2. 若状态为 `READY`，再在 GitHub Actions 手动触发 workflow。
3. 若状态为 `HOLD`，先修复失败项后再触发。

## 必查项

- workflow 文件已启用：
  - `.github/workflows/wave-c-quick-sprint-manual.yml`
- B107-B110 脚本全部存在。
- 最近一次 quick sprint bundle 报告存在。

## 回退

若触发后需停用 workflow：
- `bash scripts/toggle_wave_c_quick_sprint_workflow.sh disable`
