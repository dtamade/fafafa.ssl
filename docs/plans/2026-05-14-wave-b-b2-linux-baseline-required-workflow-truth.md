# Wave B/B2 Linux Baseline Required Workflow Truth

## Goal
删除 `wave-b-b2-manual` workflow 中与当前 handoff truth 相冲突的 `run_linux_baseline` 假可选分支，让 Linux baseline 恢复成 B2 summary/handoff 的固定前提。

## Why This Batch
- summary/handoff 入口已经强依赖 Linux summary/examples
- `prepare_wave_b_b2_handoff_bundle.sh` 也把 Linux summary 视为必需证据
- 但 workflow 仍暴露 `run_linux_baseline=false`，会把操作者引导进必坏路径

## Files
- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused workflow contract，要求 live + disabled 模板都不再暴露 `run_linux_baseline` 假开关。
2. 让合同先在当前 workflow 上 RED。
3. 最小同步两个模板：
   - 删除 dispatch input `run_linux_baseline`
   - 删除 `linux-gate` job 的条件分支
   - 删除 `Download Linux evidence` step 的条件分支
4. 跑 focused workflow contracts 与 `git diff --check`。
5. 更新 working-memory，给出简短 review 结论并提交。

## Expected Verification
- `bash -n tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- `git diff --check`
