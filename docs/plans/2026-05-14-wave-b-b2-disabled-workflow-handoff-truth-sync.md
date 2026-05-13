# Wave B/B2 Disabled Workflow Handoff Truth Sync

## Goal
把 `.github/workflows/wave-b-b2-manual.yml.disabled` 的 summary job 同步到 `prepare_wave_b_b2_handoff_bundle.sh` 单一入口，避免 disabled 模板继续保存已经被 live workflow 淘汰的 handoff 平行实现。

## Why This Batch
- live workflow 已经切到 `prepare`，disabled 模板仍在复制旧 summary 逻辑
- 当前 workflow handoff contract 只覆盖 live 文件，不能阻止 disabled 模板继续漂移
- 这个模板一旦被恢复启用或人工复制，会把旧逻辑重新带回 repo

## Files
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 扩大 workflow handoff contract，让它检查 live + disabled 双模板。
2. 观察 `.disabled` 模板在旧逻辑上的 RED 失败。
3. 最小同步 disabled 模板：
   - 删除 `MACOS_*ARGS` / `WINDOWS_EVIDENCE_ARGS`
   - 改成 `PREPARE_ARGS -> prepare_wave_b_b2_handoff_bundle.sh`
   - strict 输入映射到 `--strict`
   - final upload 增加 handoff bundle artifact
4. 跑 focused workflow contracts 与 `git diff --check`。
5. 更新 working-memory，给出简短 review 结论并提交。

## Expected Verification
- `bash -n tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- `git diff --check`
